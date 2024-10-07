#' Keras Prediction Function with Seed Control and Averaging for Return Prediction
#'
#' This function trains a Keras neural network to predict returns based on the provided training data.
#' The Keras model structure is passed in the `config` argument, and you can define custom layers, optimizers, loss functions, and callbacks (like early stopping).
#' Additionally, the function can set seeds for reproducibility and create averaged predictions using multiple seeds.
#'
#' ## Config Structure
#'
#' - `layers`: A list of layer definitions.
#' - `loss`: The loss function to use.
#' - `optimizer`: A list specifying the optimizer.
#' - `epochs`: Number of epochs to train the model.
#' - `batch_size`: Batch size for training.
#' - `verbose`: Verbosity level for Keras training.
#' - `callbacks`: List of Keras callback functions.
#' - `seeds`: A list of seeds for reproducibility and multiple prediction averaging.
#' - `python_env`: Python environment to use for training.
#'
#' @param train_data A data frame with `stock_id`, `date`, `return_label`, and features for training.
#' @param test_data A data frame with `stock_id`, `date`, and features for testing.
#' @param config A list containing the configuration for the Keras model, including layers, optimizer, loss function, and advanced features like callbacks.
#'
#' @return A tibble with `stock_id`, `date`, and averaged `pred_return` for the test data.
#' @import keras3
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows
#' @importFrom checkmate assert_data_frame assert_list
#' @importFrom cli cli_alert_info
#' @export
#'
#' @examples
#' \dontrun{
#' data(data_ml)
#' train_data_ex <- data_ml[1:100, c("stock_id", "date", "R1M_Usd", "Div_Yld", "Eps", "Mkt_Cap_12M_Usd", "Mom_11M_Usd", "Ocf", "Pb", "Vol1Y_Usd")]
#' test_data_ex <- data_ml[101:150, c("stock_id", "date", "Div_Yld", "Eps", "Mkt_Cap_12M_Usd", "Mom_11M_Usd", "Ocf", "Pb", "Vol1Y_Usd")]
#' config <- list(
#'   layers = list(
#'     list(type = "dense", units = 32, activation = "relu"),
#'     list(type = "dense", units = 16, activation = "relu"),
#'     list(type = "dense", units = 1, activation = "linear")
#'   ),
#'   loss = 'mean_squared_error',
#'   optimizer = list(name = "optimizer_adam", learning_rate = 0.001),
#'   epochs = 10,
#'   batch_size = 128,
#'   verbose = 1,
#'   seeds = c(42, 123, 456),  # Multiple seeds for averaging predictions
#'   plot_training = TRUE
#' )
#' predictions <- keras_pred(train_data_ex, test_data_ex, config)
#' print(predictions)
#' }
keras_pred <- function(train_data, test_data, config = list()) {

  # Set the specified Python environment if provided
  if (!is.null(config$python_env)) {
    reticulate::use_python(config$python_env, required = TRUE)
  }

  # Check if Keras and TensorFlow are available
  if (!reticulate::py_module_available("keras")) {
    stop("The 'keras' module is not available in the current Python environment. Please install Keras and TensorFlow.")
  }

  if (!reticulate::py_module_available("tensorflow")) {
    stop("The 'tensorflow' module is not available in the current Python environment. Please install TensorFlow.")
  }

  # Input validation
  checkmate::assert_data_frame(train_data, min.rows = 1, any.missing = FALSE)
  checkmate::assert_data_frame(test_data, min.rows = 1, any.missing = FALSE)
  checkmate::assert_list(config)

  # Extract features and response
  return_label <- colnames(train_data)[3]
  feature_names <- colnames(train_data)[4:ncol(train_data)]  # Assuming features start from 4th column

  # Convert to matrices for Keras
  train_x <- as.matrix(train_data[, feature_names])
  train_y <- as.matrix(train_data[[return_label]])
  test_x <- as.matrix(test_data[, feature_names])

  # Dynamically determine input shape based on number of features
  input_shape <- ncol(train_x)

  # Define seeds for reproducibility and prediction averaging
  seeds <- config$seeds %||% 0  # If no seeds provided, use NULL

  # Function to build and train the model
  train_model <- function(seed) {
    set_random_seed(seed, disable_gpu = FALSE) # Set seed for reproducibility
    # Build the model from the config
    model <- keras_model_sequential()

    # Loop through the config and add layers dynamically
    for (layer in config$layers) {
      if (layer$type == "dense") {
        if (length(model$layers) == 0) {
          model %>%
            layer_dense(units = layer$units, activation = layer$activation, input_shape = input_shape,
                        kernel_initializer = layer$kernel_initializer %||% NULL,
                        kernel_constraint = layer$kernel_constraint %||% NULL)
        } else {
          model %>%
            layer_dense(units = layer$units, activation = layer$activation,
                        bias_initializer = layer$bias_initializer %||% NULL,
                        kernel_regularizer = layer$kernel_regularizer %||% NULL)
        }
        if (!is.null(layer$dropout)) {
          model %>% layer_dropout(rate = layer$dropout)
        }
      }
    }

    # Extract optimizer function and its parameters
    optimizer_name <- config$optimizer$name
    optimizer_func <- match.fun(optimizer_name)
    optimizer <- do.call(optimizer_func, config$optimizer[-1])  # Remove 'name' and pass rest of the args

    # Use custom loss function if provided, else use default
    loss_function <- config$loss %||% "mean_squared_error"

    # Set the verbose level (default: 0)
    verbose_level <- config$verbose %||% 0

    # Compile the model with the optimizer, loss function, and metrics
    model %>% compile(
      loss = loss_function,
      optimizer = optimizer,
      metrics = c('mean_squared_error')
    )

    # Handle callbacks (e.g., early stopping)
    callbacks_list <- config$callbacks %||% list()

    # Train the model
    history <- model %>% fit(
      train_x, train_y,
      epochs = config$epochs,
      batch_size = config$batch_size,
      verbose = verbose_level,
      callbacks = callbacks_list
    )

    # Check if plot_training is set in config and plot the training history
    if (isTRUE(config$plot_training)) {
      plot(history)
    }

    return(model)
  }

  # Perform predictions for each seed
  prediction_list <- lapply(seeds, function(seed) {
    model <- train_model(seed)  # Train the model
    # Predict with maximum batch size for faster predictions
    predictions <- model %>% predict(test_x, batch_size = length(test_x), verbose = 0)
    return(as.vector(predictions))  # Return predictions as a vector
  })

  # If multiple predictions are made, average them
  if (length(prediction_list) > 1) {
    avg_predictions <- Reduce("+", prediction_list) / length(prediction_list)
  } else {
    avg_predictions <- prediction_list[[1]]
  }

  # Return averaged predictions in tibble format
  predictions <- tibble::tibble(
    stock_id = test_data$stock_id,
    date = test_data$date,
    pred_return = avg_predictions
  )

  cli::cli_alert_info("Keras model prediction completed.")

  return(predictions)
}
