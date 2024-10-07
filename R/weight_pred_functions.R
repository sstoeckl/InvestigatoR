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
#' @import keras3
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows
#' @importFrom checkmate assert_data_frame assert_list
#' @importFrom cli cli_alert_info
#' @export
#'
#' @examples
#' \dontrun{
#' library(keras3)
#' reticulate::use_virtualenv("C:/R/python/")
#' data(data_ml)
#' test_data_ml <- data_ml %>% filter(stock_id <= 5)
#' features <- c("Div_Yld", "Eps", "Mkt_Cap_12M_Usd", "Mom_11M_Usd", "Ocf", "Pb", "Vol1Y_Usd")
#' train_data_ex <- test_data_ml |> filter(date<="2012-12-31") |>
#'   group_by(date) |>
#'   mutate(benchmark = Mkt_Cap_3M_Usd/sum(Mkt_Cap_3M_Usd)) |> ungroup() |>
#'   select(stock_id,date,benchmark,return=R1M_Usd,all_of(features)) |> arrange(date,stock_id)
#' test_data_ex <- test_data_ml |> filter(date>"2012-12-31") |>
#'   group_by(date) |>
#'   mutate(benchmark = Mkt_Cap_3M_Usd/sum(Mkt_Cap_3M_Usd)) |> ungroup() |>
#'   select(stock_id,date,benchmark,all_of(features)) |> arrange(date,stock_id)
#'
#' config <- list(
#'   layers = list(
#'     list(type = "dense", units = 32, activation = "relu"),
#'     list(type = "dense", units = 16, activation = "relu"),
#'     list(type = "dense", units = 1, activation = "linear")
#'   ),
#'   loss = list(name = "sharpe_ratio_loss",
#'       transaction_costs = 0.001, # prevent too much turnover
#'       delta = 0.1,               # Diversification target
#'       lambda = 0.1,              # Diversification penalty multiplier
#'       leverage = 1.0,            # Target leverage
#'       eta = 0                  # Leverage penalty multiplier
#'       ),  # Custom Sharpe ratio loss
#'   optimizer = list(name = "optimizer_rmsprop", learning_rate = 0.001),
#'   metrics = list(turnover_metric(), leverage_metric(), diversification_metric(), dummy_mse_loss ),  # Custom metrics added here
#'   callbacks = list(
#'     callback_early_stopping(monitor = "loss", min_delta = 0.001, patience = 3)
#'   ),
#'   epochs = 10,
#'   batch_size = 128,
#'   verbose = 1,
#'   seeds = c(42, 123, 456),
#'   plot_training = TRUE,
#'   plot_result = TRUE
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

  # for debugging purposes:
  # train_data <- train_data_ex
  # test_data <- test_data_ex
  # Input validation
  checkmate::assert_data_frame(train_data, min.rows = 1, any.missing = FALSE)
  checkmate::assert_data_frame(test_data, min.rows = 1, any.missing = FALSE)
  checkmate::assert_list(config)

  # Extract stock_id and date
  stock_id <- train_data$stock_id
  date <- train_data$date

  # Check if benchmark is present, otherwise set defaults
  benchmark <- train_data[, 3]
  return_label <- colnames(train_data)[4]
  feature_names <- colnames(train_data)[5:ncol(train_data)]

  # Convert to matrices for Keras
  train_x <- as.matrix(train_data[, feature_names])
  train_y <- as.matrix(cbind(stock_id, as.numeric(date), benchmark, train_data[[return_label]]))  # Attach mask and benchmark to y
  test_x <- as.matrix(test_data[, feature_names])

  # Dynamically determine input shape based on number of features
  input_shape <- ncol(train_x)

  # Define seeds for reproducibility and prediction averaging
  seeds <- config$seeds %||% 0  # If no seeds provided, use NULL

  # Function to build and train the model
  train_model <- function(seed) {
    # seed<- 3 # for testing
    keras3::set_random_seed(seed) # Set seed for reproducibility
    # Build the model from the config
    # First, add the input layer explicitly using layer_input
    if (tensorflow::tf_version() < "2.11") { # not sure about the exact number
      model <- keras_model_sequential()
      model %>%
        layer_dense(units = config$layers[[1]]$units, activation = config$layers[[1]]$activation, input_shape = list(input_shape),
                    kernel_initializer = config$layers[[1]]$kernel_initializer %||% NULL,
                    kernel_constraint = config$layers[[1]]$kernel_constraint %||% NULL,
                    bias_initializer = config$layers[[1]]$bias_initializer %||% NULL,
                    kernel_regularizer = config$layers[[1]]$kernel_regularizer %||% NULL)
    } else {
      model <- keras_model_sequential(input_shape = list(input_shape))
      model %>%
        layer_dense(units = config$layers[[1]]$units, activation = config$layers[[1]]$activation,
                    kernel_initializer = config$layers[[1]]$kernel_initializer %||% NULL,
                    kernel_constraint = config$layers[[1]]$kernel_constraint %||% NULL,
                    bias_initializer = config$layers[[1]]$bias_initializer %||% NULL,
                    kernel_regularizer = config$layers[[1]]$kernel_regularizer %||% NULL)
    }

    # Loop through the config and add layers dynamically
    for (i in 2:length(config$layers)) {
      layer <- config$layers[[i]]
      if (layer$type == "dense") {
        model %>%
          layer_dense(
            units = layer$units,
            activation = layer$activation,
            kernel_initializer = layer$kernel_initializer %||% NULL,
            kernel_constraint = layer$kernel_constraint %||% NULL,
            bias_initializer = layer$bias_initializer %||% NULL,
            kernel_regularizer = layer$kernel_regularizer %||% NULL
          )

        # Add dropout layer if specified
        if (!is.null(layer$dropout)) {
          model %>%
            layer_dropout(rate = layer$dropout)
        }
      }
    }

    # Extract optimizer function and its parameters from the config
    optimizer_name <- config$optimizer$name
    optimizer_func <- match.fun(optimizer_name)
    optimizer <- do.call(optimizer_func, config$optimizer[-1])  # Remove 'name' and pass the rest of the args

    # Extract custom metrics from config (if provided)
    custom_metrics <- config$metrics %||% list()

    # Extract loss function and its parameters from the config
    loss_name <- config$loss$name
    loss_func <- match.fun(loss_name)

    # Set the verbose level (default: 0)
    verbose_level <- config$verbose %||% 0

    # Compile the model with the optimizer and loss function
    model %>% compile(
      loss = do.call(loss_func, config$loss[-1]),  # Remove 'name' and pass the rest of the args
      optimizer = optimizer,
      metrics = custom_metrics  # Add custom metrics here
    )

    # Handle callbacks (e.g., early stopping)
    callbacks_list <- config$callbacks %||% list()

    # Train the model
    history <- model %>% fit(
      train_x, train_y,
      epochs = config$epochs,
      batch_size = nrow(train_x),
      verbose = verbose_level,
      callbacks = callbacks_list
    )

    # Check if plot_training is set in config and plot the training history
    if (isTRUE(config$plot_training)) {
      plot(history)
    }

    return(list(model = model, history = history))
  }

  # Perform predictions for each seed

  histories <- list()

  prediction_list <- lapply(seeds, function(seed) {
    train_result <- train_model(seed)  # Train the model
    model <- train_result$model
    history <- train_result$history
    histories[[as.character(seed)]] <<- history  # Store history with seed as name

    # Predict with maximum batch size for faster predictions
    predictions <- model %>% predict(test_x, batch_size = nrow(test_x), verbose = 0)
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

  return_list <- list(
    predictions = predictions,
    histories = histories
  )


  # Check if plot_result is TRUE to generate summary table and combined plots
  if (isTRUE(config$plot_result)) {
    # Load necessary libraries

    # Function to convert history object to a tidy dataframe
    history_to_df <- function(history, seed) {
      # Extract metrics from history
      metrics <- as.data.frame(history$metrics)
      metrics$epoch <- 1:nrow(metrics)
      metrics_long <- pivot_longer(metrics, cols = -epoch, names_to = "metric", values_to = "value")
      metrics_long$seed <- seed
      return(metrics_long)
    }

    # Convert all histories to a single dataframe
    histories_df <- purrr::imap_dfr(histories, history_to_df)

    # Determine the maximum number of epochs across all seeds
    max_epochs <- max(histories_df$epoch)

    # Pad each seed's metrics up to max_epochs by carrying forward the last value
    histories_padded <- histories_df %>%
      group_by(seed, metric) %>%
      tidyr::complete(epoch = 1:max_epochs) %>%
      arrange(seed, metric, epoch) %>%
      tidyr::fill(value, .direction = "down") %>%
      ungroup()

    # Create the combined plot
    combined_plot <- ggplot(histories_padded, aes(x = epoch, y = value, color = as.factor(seed))) +
      geom_line(alpha = 0.7, lwd=1.2) +
      facet_wrap(~ metric, scales = "free_y", ncol = 2) +
      labs(
        title = "Training Metrics Across Seeds",
        x = "Epoch",
        y = "Metric Value",
        color = "Seed"
      ) +
      theme_dark() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(size = 12),
        strip.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10)
      )

    # Display the combined plot
    print(combined_plot)

    # Add the combined plot to the return list
    return_list$combined_plot <- combined_plot
  }

  return(return_list)
}

#' Custom Sharpe Ratio Loss Function for Keras
#'
#' This function calculates a custom Sharpe ratio-based loss for portfolio optimization using Keras,
#' with penalties for turnover, lack of diversification, and leverage deviations.
#'
#' @param y_true Tensor. A matrix where each row contains `stock_id`, `date`, `actual_return`.
#' @param y_pred Tensor. Predicted portfolio weights.
#' @param transaction_costs Numeric. Penalty applied to portfolio turnover (default: 0.001).
#' @param delta Numeric. Diversification target (default: 0.1).
#' @param lambda Numeric. Diversification penalty multiplier (default: 0.1).
#' @param leverage Numeric. Target portfolio leverage (default: 1.0).
#' @param eta Numeric. Leverage penalty multiplier (default: 0.1).
#'
#' @return A scalar loss value that combines the negative Sharpe ratio with penalties for turnover, diversification, and leverage.
#' @import keras3
#' @import tensorflow
#'
#' @examples
#' \dontrun{
#' library(keras3)
#' library(tensorflow)
#'
#' # Example: simple portfolio with 3 stocks and 2 dates
#' example_data <- data.frame(
#'   stock_id = c(1, 1, 2, 2, 3, 3),
#'   date = c(1, 2, 1, 2, 1, 2),
#'   benchmark=rep(0, 6),
#'   actual_return = c(0.01, 0.02, -0.01, 0.01, 0.03, -0.02)
#' )
#'
#' # Sample predicted weights
#' predicted_weights <- c(0.5, 0.6, 0.3, 0.2, 0.2, 0.2)
#'
#' # Convert data to TensorFlow tensors
#' y_true <- tf$constant(as.matrix(example_data), dtype = "float32")
#' y_pred <- tf$constant(predicted_weights, dtype = "float32")
#'
#' # Call the custom loss function
#' loss_value <- sharpe_ratio_loss(
#'   y_true = y_true,
#'   y_pred = y_pred,
#'   transaction_costs = 0.005,
#'   delta = 0.1,     # Diversification penalty weight
#'   lambda = 0.1,    # Diversification penalty multiplier
#'   leverage = 1.0,  # Target leverage
#'   eta = 0.1        # Leverage penalty multiplier
#' )
#'
#' # Print the calculated loss value
#' print(loss_value)
#' }
# Loss function wrapper
sharpe_ratio_loss <- function(transaction_costs = 0.001, delta = 0.1, lambda = 0.1, leverage = 1.0, eta = 0.1) {
  function(y_true, y_pred) {
    # Extract stock_id, date, and actual_return from y_true
    stock_ids <- tf$cast(y_true[, 1], "int32")
    dates <- tf$cast(y_true[, 2], "int32")
    actual_returns <- y_true[, 4]

    # Get unique stock_id and date vectors for reshaping
    unique_stock_ids <- tf$unique(stock_ids)$y
    unique_dates <- tf$unique(dates)$y

    # Determine the number of unique stocks and dates
    num_dates <- tf$shape(unique_dates)[1]
    num_stocks <- tf$shape(unique_stock_ids)[1]

    # Get the indices for stock_id and date in the unique lists using argmax
    stock_indices <- tf$map_fn(
      function(x) tf$squeeze(tf$cast(tf$where(tf$equal(unique_stock_ids, x)), "int32")),
      stock_ids
    )
    date_indices <- tf$map_fn(
      function(x) tf$squeeze(tf$cast(tf$where(tf$equal(unique_dates, x)), "int32")),
      dates
    )

    # Print the shapes to debug
    #tf$print("Shape of actual_returns before reshape:", tf$shape(actual_returns))
    actual_returns <- tf$reshape(actual_returns, shape = list(-1L))  # Flatten to 1D
    #tf$print("Shape of actual_returns after reshape:", tf$shape(actual_returns))

    #tf$print("Shape of y_pred before reshape:", tf$shape(y_pred))
    y_pred <- tf$reshape(y_pred, shape = list(-1L))  # Flatten to 1D
    #tf$print("Shape of y_pred after reshape:", tf$shape(y_pred))

    # Stack date_indices and stock_indices to create [N, 2] indices
    indices <- tf$stack(list(date_indices, stock_indices), axis = -1)

    # Print indices shape to verify it has [N, 2] shape
    #tf$print("Shape of stock_indices:", tf$shape(stock_indices))
    #tf$print("Shape of date_indices:", tf$shape(date_indices))
    #tf$print("Shape of indices:", tf$shape(indices))

    # Scatter the actual returns and predicted weights into tensors
    stock_returns_tensor <- tf$scatter_nd(indices, actual_returns, shape = list(num_dates, num_stocks))
    weights_tensor <- tf$scatter_nd(indices, y_pred, shape = list(num_dates, num_stocks))

    # Ensure the scatter operation produces correct tensors
    #tf$print("Shape of stock_returns_tensor:", tf$shape(stock_returns_tensor))
    #tf$print("Shape of weights_tensor:", tf$shape(weights_tensor))

    # Apply weight normalization based on the sum of absolute weights
    weight_sum_per_date <- tf$reduce_sum(tf$abs(weights_tensor), axis = -1L, keepdims = TRUE)
    normalized_weights <- weights_tensor / (weight_sum_per_date + tf$constant(.Machine$double.eps, dtype = "float32"))

    # Calculate portfolio returns as the sum of weighted stock returns per date
    portfolio_returns <- tf$reduce_sum(normalized_weights * stock_returns_tensor, axis = -1L)

    # Calculate turnover: absolute change in weights from one time step to the next
    zero_weights <- tf$zeros_like(normalized_weights[1:1, ])
    padded_weights <- tf$concat(list(zero_weights, normalized_weights[1:(num_dates - 1), ]), axis = 0L)
    weight_differences <- tf$abs(normalized_weights - padded_weights)
    turnover <- tf$reduce_sum(weight_differences, axis = -1L)

    # Adjust portfolio returns by subtracting the turnover penalty
    adjusted_portfolio_returns <- portfolio_returns - (transaction_costs * turnover)

    # Calculate the Sharpe ratio
    portfolio_return_mean <- tf$reduce_mean(adjusted_portfolio_returns)
    portfolio_return_std <- tf$math$reduce_std(adjusted_portfolio_returns)
    sharpe_ratio <- portfolio_return_mean / (portfolio_return_std + tf$constant(.Machine$double.eps, dtype = "float32"))

    # Diversification penalty: penalize portfolios with low diversification
    variance_penalty <- lambda * (tf$reduce_mean(tf$abs(tf$reduce_sum(tf$square(normalized_weights), axis = -1L))) - tf$cast(1/num_stocks, "float32"))

    # Leverage penalty: penalize deviations from target leverage
    leverage_penalty <- eta * tf$reduce_mean(tf$square(tf$reduce_sum(tf$abs(normalized_weights), axis = -1L) - leverage))

    # Total penalty combining diversification and leverage penalties
    total_penalty <- variance_penalty + leverage_penalty

    # Negative Sharpe ratio as the loss (to maximize Sharpe ratio)
    loss <- -sharpe_ratio + total_penalty

    return(loss)
  }
}

# Custom Turnover Metric for Keras
#'
#' This function calculates and reports the portfolio turnover during training.
#'
#' @return A scalar turnover value for the current batch of data.
#' @import keras3
#' @import tensorflow
#'
turnover_metric <- function() {
  custom_metric("turnover_metric", function(y_true, y_pred) {
    # Extract stock_id, date, and actual_return from y_true
    stock_ids <- tf$cast(y_true[, 1], "int32")
    dates <- tf$cast(y_true[, 2], "int32")

    # Get unique stock_id and date vectors for reshaping
    unique_stock_ids <- tf$unique(stock_ids)$y
    unique_dates <- tf$unique(dates)$y

    # Determine the number of unique stocks and dates
    num_dates <- tf$shape(unique_dates)[1]
    num_stocks <- tf$shape(unique_stock_ids)[1]

    # Get the indices for stock_id and date in the unique lists
    stock_indices <- tf$map_fn(
      function(x) tf$cast(tf$reshape(tf$where(tf$equal(unique_stock_ids, x)), list()), "int32"),
      stock_ids,
      dtype = tf$int32
    )
    date_indices <- tf$map_fn(
      function(x) tf$cast(tf$reshape(tf$where(tf$equal(unique_dates, x)), list()), "int32"),
      dates,
      dtype = tf$int32
    )

    # Stack the indices tensor for scatter_nd
    indices <- tf$stack(list(date_indices, stock_indices), axis = -1)

    # Reshape y_pred to be compatible with the indices
    y_pred_flat <- tf$reshape(y_pred, shape = list(-1L))  # Flatten y_pred to be a vector

    # Scatter the predicted weights into the tensor
    weights_tensor <- tf$scatter_nd(indices, y_pred_flat, shape = list(num_dates, num_stocks))

    # Apply weight normalization based on the sum of absolute weights
    weight_sum_per_date <- tf$reduce_sum(tf$abs(weights_tensor), axis = -1L, keepdims = TRUE)
    normalized_weights <- weights_tensor / (weight_sum_per_date + tf$constant(.Machine$double.eps, dtype = "float32"))

    # Calculate turnover: absolute change in weights from one time step to the next
    zero_weights <- tf$zeros_like(normalized_weights[1:1, ])
    padded_weights <- tf$concat(list(zero_weights, normalized_weights[1:(num_dates - 1), ]), axis = 0L)
    weight_differences <- tf$abs(normalized_weights - padded_weights)
    turnover <- tf$reduce_sum(weight_differences, axis = -1L)

    # Return the mean turnover value
    tf$reduce_mean(turnover)
  })
}

# Leverage Metric for Keras
#'
#' This function calculates and reports the leverage of the portfolio during training.
#' Leverage is defined as the sum of absolute portfolio weights for each time period.
#'
#' @return A scalar leverage value for the current batch of data.
#' @import keras3
#' @import tensorflow
#'
leverage_metric <- function() {
  custom_metric("leverage_metric", function(y_true, y_pred) {
    # Extract stock_id and date from y_true
    stock_ids <- tf$cast(y_true[, 1], "int32")
    dates <- tf$cast(y_true[, 2], "int32")

    # Get unique stock_id and date vectors for reshaping
    unique_stock_ids <- tf$unique(stock_ids)$y
    unique_dates <- tf$unique(dates)$y

    # Determine the number of unique stocks and dates
    num_dates <- tf$shape(unique_dates)[1]
    num_stocks <- tf$shape(unique_stock_ids)[1]

    # Get the indices for stock_id and date in the unique lists
    stock_indices <- tf$map_fn(
      function(x) tf$cast(tf$reshape(tf$where(tf$equal(unique_stock_ids, x)), list()), "int32"),
      stock_ids,
      dtype = tf$int32
    )
    date_indices <- tf$map_fn(
      function(x) tf$cast(tf$reshape(tf$where(tf$equal(unique_dates, x)), list()), "int32"),
      dates,
      dtype = tf$int32
    )

    # Stack the indices tensor for scatter_nd
    indices <- tf$stack(list(date_indices, stock_indices), axis = -1)

    # Reshape y_pred to be compatible with the indices
    y_pred_flat <- tf$reshape(y_pred, shape = list(-1L))  # Flatten y_pred to be a vector

    # Scatter the predicted weights into the tensor
    weights_tensor <- tf$scatter_nd(indices, y_pred_flat, shape = list(num_dates, num_stocks))

    # Calculate leverage: sum of absolute weights per date
    leverage <- tf$reduce_sum(tf$abs(weights_tensor), axis = -1L)

    # Return the mean leverage value
    tf$reduce_mean(leverage)
  })
}

# Diversification Metric for Keras (Herfindahl-Hirschman Index)
#'
#' This function calculates and reports the diversification of the portfolio during training,
#' using the Herfindahl-Hirschman Index (HHI), which is the sum of the squared portfolio weights.
#'
#' @return A scalar diversification value (HHI) for the current batch of data.
#' @import keras3
#' @import tensorflow
#'
diversification_metric <- function() {
  custom_metric("diversification_metric", function(y_true, y_pred) {
    # Extract stock_id and date from y_true
    stock_ids <- tf$cast(y_true[, 1], "int32")
    dates <- tf$cast(y_true[, 2], "int32")

    # Get unique stock_id and date vectors for reshaping
    unique_stock_ids <- tf$unique(stock_ids)$y
    unique_dates <- tf$unique(dates)$y

    # Determine the number of unique stocks and dates
    num_dates <- tf$shape(unique_dates)[1]
    num_stocks <- tf$shape(unique_stock_ids)[1]

    # Get the indices for stock_id and date in the unique lists
    stock_indices <- tf$map_fn(
      function(x) tf$cast(tf$reshape(tf$where(tf$equal(unique_stock_ids, x)), list()), "int32"),
      stock_ids,
      dtype = tf$int32
    )
    date_indices <- tf$map_fn(
      function(x) tf$cast(tf$reshape(tf$where(tf$equal(unique_dates, x)), list()), "int32"),
      dates,
      dtype = tf$int32
    )

    # Stack the indices tensor for scatter_nd
    indices <- tf$stack(list(date_indices, stock_indices), axis = -1)

    # Reshape y_pred to be compatible with the indices
    y_pred_flat <- tf$reshape(y_pred, shape = list(-1L))  # Flatten y_pred to be a vector

    # Scatter the predicted weights into the tensor
    weights_tensor <- tf$scatter_nd(indices, y_pred_flat, shape = list(num_dates, num_stocks))

    # Calculate diversification using Herfindahl-Hirschman Index (sum of squared weights)
    hhi <- tf$reduce_sum(tf$square(weights_tensor), axis = -1L)

    # Return the mean diversification (HHI) value
    tf$reduce_mean(hhi)
  })
}

# Dummy MSE Loss Function with Stock ID, Date, and Actual Return
#'
#' This function calculates the Mean Squared Error (MSE) between the actual returns (from column 4 of y_true)
#' and the predicted values (y_pred). It is used as a dummy function for debugging the custom loss function
#' input structure.
#'
#' @param y_true A matrix where the 4th column contains the actual returns.
#' @param y_pred A vector of predicted portfolio weights/returns.
#'
#' @return The calculated MSE loss value.
#' @import tensorflow
#'
dummy_mse_loss <- function(y_true, y_pred) {
  # Extract the actual returns from the 4th column of y_true
  actual_returns <- y_true[, 4]

  # Calculate Mean Squared Error (MSE)
  mse <- tf$reduce_mean(tf$square(actual_returns - y_pred))

  return(mse)
}

########################### ACTIVATIONS
#' Box Constraints Activation with Sigmoid Scaling
#'
#' This activation function maps the output using a sigmoid function to ensure that the predicted weights fall
#' within a specified range (`min_weight`, `max_weight`).
#'
#' @param min_weight Minimum weight to map to (default: -1.0).
#' @param max_weight Maximum weight to map to (default: 1.0).
#' @return A Keras activation function that maps the input to the desired range using the sigmoid function.
#' @export
#'
#' @examples
#' \dontrun{
#' data(data_ml)
#' test_data_ml <- data_ml %>% filter(stock_id <= 5)
#' train_data_ex <- test_data_ml |> filter(date<="2012-12-31") |> arrange(date, stock_id)
#' test_data_ex <- test_data_ml |> filter(date>"2012-12-31") |> arrange(date, stock_id)
#' config_box_sigmoid <- list(
#'   layers = list(
#'     list(type = "dense", units = 32, activation = "relu"),
#'     list(type = "dense", units = 16, activation = "relu"),
#'     list(type = "dense", units = 1, activation = activation_box_sigmoid(min_weight=0.5,max_weight=1))
#'   ),
#'   loss = "dummy_mse_loss",
#'   optimizer = list(name = "optimizer_adam", learning_rate = 0.001),
#'   epochs = 10,
#'   batch_size = 128,
#'   verbose = 0,
#'   seeds = c(42)
#' )
#' weights <- keras_weights(train_data_ex, test_data_ex, config_box_sigmoid)
#' print(weights)
#' }
activation_box_sigmoid <- function(min_weight = -1.0, max_weight = 1.0) {
  function(x) {
    # Map to the desired range using sigmoid
    x_mapped <- min_weight + (max_weight - min_weight) * op_sigmoid(x)
    return(x_mapped)
  }
}
#' Box Constraints Activation with Sigmoid Scaling
#'
#' This activation function maps the output using a sigmoid function to ensure that the predicted weights fall
#' within a specified range (`min_weight`, `max_weight`).
#'
#' @param min_weight Minimum weight to map to (default: -1.0).
#' @param max_weight Maximum weight to map to (default: 1.0).
#' @return A Keras activation function that maps the input to the desired range using the sigmoid function.
#' @export
#'
#' @examples
#' \dontrun{
#' data(data_ml)
#' test_data_ml <- data_ml %>% filter(stock_id <= 5)
#' train_data_ex <- test_data_ml |> filter(date<="2012-12-31") |> arrange(date, stock_id)
#' test_data_ex <- test_data_ml |> filter(date>"2012-12-31") |> arrange(date, stock_id)
#' config_box_tanh <- list(
#'   layers = list(
#'     list(type = "dense", units = 32, activation = "relu"),
#'     list(type = "dense", units = 16, activation = "relu"),
#'     list(type = "dense", units = 1, activation = activation_box_tanh(min_weight=0.5,max_weight=1))
#'   ),
#'   loss = "dummy_mse_loss",
#'   optimizer = list(name = "optimizer_adam", learning_rate = 0.001),
#'   epochs = 10,
#'   batch_size = 128,
#'   verbose = 0,
#'   seeds = c(42)
#' )
#' weights <- keras_weights(train_data_ex, test_data_ex, config_box_tanh)
#' print(weights)
#' }
activation_box_tanh <- function(min_weight = -1.0, max_weight = 1.0) {
  function(x) {
    # Map to the desired range using sigmoid
    x_mapped <- min_weight + (max_weight - min_weight) * op_tanh(x)
    return(x_mapped)
  }
}


#' Box Constraints and Sum Constraints Activation with Sigmoid Scaling
#'
#' This activation function first maps the output using a sigmoid function to ensure that the predicted weights fall
#' within a specified range (`min_weight`, `max_weight`), then normalizes the weights to ensure they sum to `target_sum`.
#'
#' @param min_weight Minimum weight to map to (default: -1.0).
#' @param max_weight Maximum weight to map to (default: 1.0).
#' @param target_sum The target sum of the weights (default: 0.0).
#' @return A Keras activation function that maps the input to the desired range using the sigmoid function and normalizes the sum.
#' @export
#'
#' @examples
#' \dontrun{
#' data(data_ml)
#' test_data_ml <- data_ml %>% filter(stock_id <= 5)
#' train_data_ex <- test_data_ml |> filter(date<="2012-12-31") |> arrange(date, stock_id)
#' test_data_ex <- test_data_ml |> filter(date>"2012-12-31") |> arrange(date, stock_id)
#' config_box_sum_sigmoid <- list(
#'   layers = list(
#'     list(type = "dense", units = 32, activation = "relu"),
#'     list(type = "dense", units = 16, activation = "relu"),
#'     list(type = "dense", units = 1, activation = activation_box_sum_sigmoid(min_weight=0.5,max_weight=1,target_sum=1))
#'   ),
#'   loss = "dummy_mse_loss",
#'   optimizer = list(name = "optimizer_adam", learning_rate = 0.001),
#'   epochs = 10,
#'   batch_size = 128,
#'   verbose = 1,
#'   seeds = c(42)
#' )
#' weights <- keras_weights(train_data_ex, test_data_ex, config_box_sum_sigmoid)
#' print(weights)
#' }
activation_box_sum_sigmoid <- function(min_weight = -1.0, max_weight = 1.0, target_sum = 0) {
  function(x) {
    # Map to the desired range using sigmoid
    x_mapped <- min_weight + (max_weight - min_weight) * op_sigmoid(x)
    # Normalize the weights to ensure they sum to target_sum
    weight_sum <- op_sum(x_mapped, axis = -1, keepdims = TRUE)
    return(x_mapped - weight_sum + target_sum)
  }
}

#' Sum Constraints Activation with Sigmoid Scaling
#'
#' This activation function normalizes the predicted weights to ensure they sum to `target_sum` after applying the sigmoid function.
#'
#' @param min_weight Minimum weight (default: -1.0).
#' @param max_weight Maximum weight (default: 1.0).
#' @param target_sum The target sum of the weights (default: 0.0).
#' @return A Keras activation function that normalizes the weights to meet the target sum.
#' @export
#'
#' @examples
#' \dontrun{
#' data(data_ml)
#' test_data_ml <- data_ml %>% filter(stock_id <= 5)
#' train_data_ex <- test_data_ml |> filter(date<="2012-12-31") |> arrange(date, stock_id)
#' test_data_ex <- test_data_ml |> filter(date>"2012-12-31") |> arrange(date, stock_id)
#' config_sum_sigmoid <- list(
#'   layers = list(
#'     list(type = "dense", units = 32, activation = "relu"),
#'     list(type = "dense", units = 16, activation = "relu"),
#'     list(type = "dense", units = 5, activation = activation_sum_sigmoid(target_sum = 0))
#'   ),
#'   loss = "dummy_mse_loss",
#'   optimizer = list(name = "optimizer_adam", learning_rate = 0.001),
#'   epochs = 10,
#'   batch_size = 128,
#'   verbose = 1,
#'   seeds = c(42)
#' )
#' weights <- keras_weights(train_data_ex, test_data_ex, config_sum_sigmoid)
#' print(weights)
#' }
activation_sum_sigmoid <- function(target_sum = 0.0) {
  function(x) {
    # Map to the desired range using sigmoid
    x_mapped <- op_sigmoid(x)
    # Normalize the weights to ensure they sum to target_sum
    weight_sum <- op_sum(x_mapped, axis = -1, keepdims = TRUE)
    return(x_mapped - weight_sum + target_sum)
  }
}

