#' Keras Weight Prediction Function with Masking and Benchmark Handling
#'
#' This function trains a Keras neural network to predict portfolio weights based on the provided training data.
#' The Keras model structure is passed in the `config` argument, and you can define custom layers, optimizers, loss functions, and callbacks (like early stopping).
#' Additionally, this function can handle masking and benchmark weights, which will be passed to the loss function as part of `y_true`.
#'
#' ## Config Structure
#'
#' - `layers`: A list of layer definitions.
#' - `loss`: The loss function to use (e.g., Sharpe ratio loss).
#' - `optimizer`: A list specifying the optimizer.
#' - `epochs`: Number of epochs to train the model.
#' - `batch_size`: Batch size for training.
#' - `verbose`: Verbosity level for Keras training.
#' - `callbacks`: List of Keras callback functions.
#' - `seeds`: A list of seeds for reproducibility and multiple prediction averaging.
#' - `python_env`: Python environment to use for training.
#'
#' @param train_data A data frame with columns: `stock_id`, `date`, `benchmark`, `mask`, `actual_return`, and features.
#' @param test_data A data frame with columns: `stock_id`, `date`, and features.
#' @param config A list containing the configuration for the Keras model, including layers, optimizer, loss function, and advanced features like callbacks.
#'
#' @return A tibble with `stock_id`, `date`, and predicted portfolio weights for the test data.
#' @import keras
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
#'   loss = sharpe_ratio_loss_keras,  # Custom Sharpe ratio loss
#'   optimizer = list(name = "optimizer_adam", learning_rate = 0.001),
#'   epochs = 10,
#'   batch_size = 128,
#'   verbose = 1,
#'   seeds = c(42, 123, 456),
#'   plot_training = TRUE
#' )
#' weights <- keras_weights(train_data_ex, test_data_ex, config)
#' print(weights)
#' }
keras_weights <- function(train_data, test_data, config = list()) {
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

  # Extract stock_id and date
  stock_id <- train_data$stock_id
  date <- train_data$date

  # Check if benchmark and mask are present, otherwise set defaults
  benchmark <- train_data[, 3]
  mask <- train_data[, 4]
  return_label <- colnames(train_data)[5]
  feature_names <- colnames(train_data)[6:ncol(train_data)]

  # Convert to matrices for Keras
  train_x <- as.matrix(train_data[, feature_names])
  train_y <- as.matrix(cbind(stock_id, as.numeric(date), train_data[[return_label]], mask, benchmark))  # Attach mask and benchmark to y
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
    pred_weight = avg_predictions
  )

  cli::cli_alert_info("Keras model weight prediction completed.")

  return(predictions)
}


#' Custom Sharpe Ratio Loss with Constraints for Keras Models
#'
#' This function calculates the Sharpe ratio for portfolio optimization using Keras, incorporating constraints
#' like leverage, weight limits, and turnover. It supports both long-only and long-short portfolios.
#' The loss function is designed to maximize the Sharpe ratio while applying penalties for turnover and weight constraints.
#'
#' ## Arguments
#' - `turnover_penalty_weight`: Penalty applied to turnover in the portfolio (default: 0.01).
#' - `max_weight`: Maximum allowable weight for any single stock (default: 0.1).
#' - `min_weight`: Minimum allowable weight for any single stock (default: 0).
#' - `weights_sum`: Target sum of portfolio weights (default: 1 for long-only, 0 for market-neutral).
#' - `abs_weights_sum`: Maximum absolute sum of portfolio weights to control leverage (default: 1 for long-only, higher for long-short).
#'
#' @param y_true Matrix with columns for stock_id, date, actual_return, and optional mask.
#' @param y_pred Predicted portfolio weights.
#' @param turnover_penalty_weight Numeric. Penalty for portfolio turnover (default: 0.01).
#' @param max_weight Numeric. Maximum weight for a single stock (default: 0.1).
#' @param min_weight Numeric. Minimum weight for a single stock (default: 0).
#' @param weights_sum Numeric. Target sum of portfolio weights (default: 1).
#' @param abs_weights_sum Numeric. Maximum absolute sum of portfolio weights (default: 1).
#'
#' @return Numeric. The loss value (negative Sharpe ratio) to minimize during training.
#' @import keras
#' @import tensorflow
#' @export
#'
#' @examples
#' # Example: Simple portfolio with 3 stocks and 3 dates
#' # Generate a small example dataset
#' example_data <- data.frame(
#'   stock_id = c(1, 1, 2, 2, 3, 3,1,2,3),
#'     date = c(1, 2, 1, 2, 1, 2,3,3,3),
#'       actual_return = c(0.01, 0.02, -0.01, 0.01, 0.03, -0.02,0.04,0.05,0.06),
#'       mask = c(1, 1, 1, 1, 1, 1,0,0,0)
#'       )
#' # Predicted weights (this would come from your model in practice)
#' predicted_weights <- c(0.5, 0.6, 0.3, 0.2, 0.2, 0.2,0.,0.9,0.8)
#'
#' # Convert data to TensorFlow tensors
#' y_true <- tf$constant(as.matrix(example_data), dtype = "float32")
#' y_pred <- tf$constant(predicted_weights, dtype = "float32")
#'
#' # Calculate loss using the custom Sharpe ratio loss function
#' loss_value <- sharpe_ratio_loss_keras(
#'   y_true = y_true,
#'   y_pred = y_pred,
#'   turnover_penalty_weight = 0.005,
#'   max_weight = 0.5,
#'   min_weight = -0.5,
#'   weights_sum = 1,
#'   abs_weights_sum = 1
#' )
#' print(loss_value)
sharpe_ratio_loss_keras <- function(
    y_true, y_pred,
    turnover_penalty_weight = 0.01,
    max_weight = 0.1,
    min_weight = 0,
    weights_sum = 1,
    abs_weights_sum = 1
) {
  # Extract stock_id, date, actual_return, and mask from y_true
  stock_ids <- tf$cast(y_true[, 1], dtype = "int32")
  dates <- tf$cast(y_true[, 2], dtype = "int32")
  actual_returns <- y_true[, 3]
  mask <- y_true[, 4]

  # Create unique stock_id and date vectors for reshaping
  unique_stock_ids <- tf$unique(stock_ids)$y
  unique_dates <- tf$unique(dates)$y

  # Create a placeholder tensor to hold all stock-date combinations
  num_dates <- tf$shape(unique_dates)[1]
  num_stocks <- tf$shape(unique_stock_ids)[1]

  # Get the indices for stock_id and date in the unique lists
  stock_indices <- tf$map_fn(function(x) tf$cast(tf$where(unique_stock_ids == x), dtype = "int32"), stock_ids)
  date_indices <- tf$map_fn(function(x) tf$cast(tf$where(unique_dates == x), dtype = "int32"), dates)

  # Stack the indices tensor for scatter_nd
  indices <- tf$stack(list(date_indices, stock_indices), axis = -1)
  indices <- tf$reshape(indices, shape = list(tf$shape(y_pred)[1], 2L))

  # Create zero-initialized tensors for stock returns and weights
  stock_returns_tensor <- tf$zeros(c(num_dates, num_stocks))
  weights_tensor <- tf$zeros(c(num_dates, num_stocks))

  # Use scatter_nd to place values from y_true and y_pred into the tensors
  stock_returns_tensor <- tf$scatter_nd(indices, actual_returns, shape = c(num_dates, num_stocks))
  weights_tensor <- tf$scatter_nd(indices, y_pred, shape = c(num_dates, num_stocks))

  # Apply mask to filter relevant stocks
  mask_tensor <- tf$scatter_nd(indices, mask, shape = c(num_dates, num_stocks))
  stock_returns_tensor <- stock_returns_tensor * mask_tensor
  weights_tensor <- weights_tensor * mask_tensor

  # Apply weight constraints (min and max weights)
  clipped_weights <- tf$clip_by_value(weights_tensor, min_weight, max_weight)

  # Normalize the weights so they sum to the specified value for each date
  weights_sum_per_date <- tf$reduce_sum(clipped_weights, axis = 1L, keepdims = TRUE)
  normalized_weights <- clipped_weights / (weights_sum_per_date + k_epsilon())

  # Calculate portfolio returns
  portfolio_returns <- tf$reduce_sum(normalized_weights * stock_returns_tensor, axis = 1L)

  # Calculate turnover: absolute change in weights from one time step to the next
  zero_weights <- tf$zeros_like(normalized_weights[1L, , drop = FALSE])
  padded_weights <- tf$concat(list(zero_weights, normalized_weights[1L:(num_dates-1L), ]), axis = 0L)
  weight_differences <- tf$abs(tf$subtract(normalized_weights, padded_weights))
  turnover <- tf$reduce_sum(weight_differences, axis = 1L)

  # Adjust portfolio returns by subtracting the turnover penalty
  adjusted_portfolio_returns <- portfolio_returns - (turnover_penalty_weight * turnover)

  # Calculate the Sharpe ratio
  portfolio_return_mean <- tf$reduce_mean(adjusted_portfolio_returns)
  portfolio_return_std <- tf$math$reduce_std(adjusted_portfolio_returns)
  sharpe_ratio <- portfolio_return_mean / (portfolio_return_std + k_epsilon())

  # Negative Sharpe ratio as the loss (to maximize Sharpe ratio)
  loss <- -sharpe_ratio

  return(loss)
}

#' Dummy MSE Loss Function with Stock ID, Date, and Actual Return
#'
#' This function calculates the Mean Squared Error (MSE) between the actual returns (from column 3 of y_true)
#' and the predicted values (y_pred). It is used as a dummy function for debugging the custom loss function
#' input structure.
#'
#' @param y_true A matrix where the 3rd column contains the actual returns.
#' @param y_pred A vector of predicted portfolio weights/returns.
#'
#' @return The calculated MSE loss value.
#' @export
dummy_mse_loss <- function(y_true, y_pred) {
  # Extract the actual returns from the 3rd column of y_true
  stock_ids <- tf$cast(y_true[, 1], dtype = "int32")
  dates <- tf$cast(y_true[, 2], dtype = "int32")
  actual_returns <- y_true[, 3]
  mask <- y_true[, 4]

  # Calculate Mean Squared Error (MSE)
  #mse <- tf$reduce_mean(tf$square(actual_returns - y_pred))
  mse <- k_mean(k_square(actual_returns - y_pred))

  return(mse)
}
