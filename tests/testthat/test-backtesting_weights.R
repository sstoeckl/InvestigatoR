library(testthat)
library(tidyverse)
library(reticulate)
library(tensorflow)
library(keras3)
reticulate::use_virtualenv("C:/R/python/")
reticulate::py_config()

# Load the data_ml dataset
# Replace this with the actual loading mechanism if different
data("data_ml")

# Create a small subset of data_ml for testing
test_data_ml <- data_ml %>%
  filter(stock_id <= 5)

# Define common parameters for testing
return_label <- "R1M_Usd"
features <- c("Div_Yld", "Eps", "Mkt_Cap_12M_Usd", "Mom_11M_Usd", "Ocf", "Pb", "Vol1Y_Usd")
rolling <- FALSE
window_size <- "5 years"
step_size <- "1 year"
offset <- "1 month"
in_sample <- TRUE

train_data_ex <- test_data_ml %>%
  filter(date <= "2010-12-31")

test_data_ex <- test_data_ml %>%
  filter(date <= "2010-12-31")

# Create a new test file named `test-custom-loss-metrics.R`
# Place this file in the `tests/testthat/` directory of your R package or project

library(testthat)

test_that("sharpe_ratio_loss functions correctly", {
  # Define the configuration for sharpe_ratio_loss
  config_sharpe <- list(
    layers = list(
      list(type = "dense", units = 32, activation = "relu"),
      list(type = "dense", units = 16, activation = "relu"),
      list(type = "dense", units = 1, activation = "linear")
    ),
    loss = list(
      name = "sharpe_ratio_loss",
      transaction_costs = 0.005,  # Example parameter
      delta = 0.1,
      lambda = 0.1,
      leverage = 1.0,
      eta = 0.1
    ),
    optimizer = list(
      name = "optimizer_adam",
      learning_rate = 0.001
    ),
    metrics = list(
      distance_from_benchmark_l1_metric(),
      distance_from_benchmark_l2_metric()
    ),
    callbacks = list(
      callback_early_stopping(monitor = "loss", min_delta = 0.001, patience = 3)
    ),
    epochs = 10,
    batch_size = 128,
    verbose = 0,  # Set to 1 for detailed output
    seeds = c(42),
    plot_training = FALSE,
    plot_result = FALSE
  )

  # Invoke keras_weights with the sharpe_ratio_loss config
  result_sharpe <- tryCatch({
    keras_weights(train_data_ex, test_data_ex, config_sharpe)
  }, error = function(e) {
    fail(paste("keras_weights failed with sharpe_ratio_loss:", e$message))
  })

  # Extract the training history
  history_sharpe <- result_sharpe$histories[[as.character(config_sharpe$seeds[[1]])]]

  # Ensure that history_sharpe is not NULL
  expect_true(!is.null(history_sharpe), info = "History object is NULL for sharpe_ratio_loss.")

  # Extract final loss and metrics
  final_loss <- tail(history_sharpe$metrics$loss, n = 1)
  final_l1 <- tail(history_sharpe$metrics$distance_from_benchmark_l1, n = 1)
  final_l2 <- tail(history_sharpe$metrics$distance_from_benchmark_l2, n = 1)

  # Assertions
  expect_true(is.finite(final_loss), info = "Final loss is not finite for sharpe_ratio_loss.")
  expect_true(is.finite(final_l1) && final_l1 >= 0, info = "Final L1 distance is not finite or negative for sharpe_ratio_loss.")
  expect_true(is.finite(final_l2) && final_l2 >= 0, info = "Final L2 distance is not finite or negative for sharpe_ratio_loss.")

  # Optionally, check that loss decreased over epochs
  expect_true(all(diff(history_sharpe$metrics$loss) <= 0), info = "Loss did not consistently decrease for sharpe_ratio_loss.")
})

test_that("sharpe_ratio_difference_loss functions correctly", {
  # Define the configuration for sharpe_ratio_difference_loss
  config_sharpe_diff <- list(
    layers = list(
      list(type = "dense", units = 32, activation = "relu"),
      list(type = "dense", units = 16, activation = "relu"),
      list(type = "dense", units = 1, activation = "linear")
    ),
    loss = list(
      name = "sharpe_ratio_difference_loss",
      lambda_l1 = 0.01,
      lambda_l2 = 0.01
    ),
    optimizer = list(
      name = "optimizer_adam",
      learning_rate = 0.001
    ),
    metrics = list(
      distance_from_benchmark_l1_metric(),
      distance_from_benchmark_l2_metric()
    ),
    callbacks = list(
      callback_early_stopping(monitor = "loss", min_delta = 0.001, patience = 3)
    ),
    epochs = 10,
    batch_size = 128,
    verbose = 0,
    seeds = c(42),
    plot_training = FALSE,
    plot_result = FALSE
  )

  # Invoke keras_weights with the sharpe_ratio_difference_loss config
  result_sharpe_diff <- tryCatch({
    keras_weights(train_data_ex, test_data_ex, config_sharpe_diff)
  }, error = function(e) {
    fail(paste("keras_weights failed with sharpe_ratio_difference_loss:", e$message))
  })

  # Extract the training history
  history_sharpe_diff <- result_sharpe_diff$histories[[as.character(config_sharpe_diff$seeds[[1]])]]

  # Ensure that history_sharpe_diff is not NULL
  expect_true(!is.null(history_sharpe_diff), info = "History object is NULL for sharpe_ratio_difference_loss.")

  # Extract final loss and metrics
  final_loss <- tail(history_sharpe_diff$metrics$loss, n = 1)
  final_l1 <- tail(history_sharpe_diff$metrics$distance_from_benchmark_l1, n = 1)
  final_l2 <- tail(history_sharpe_diff$metrics$distance_from_benchmark_l2, n = 1)

  # Assertions
  expect_true(is.finite(final_loss), info = "Final loss is not finite for sharpe_ratio_difference_loss.")
  expect_true(is.finite(final_l1) && final_l1 >= 0, info = "Final L1 distance is not finite or negative for sharpe_ratio_difference_loss.")
  expect_true(is.finite(final_l2) && final_l2 >= 0, info = "Final L2 distance is not finite or negative for sharpe_ratio_difference_loss.")

  # Optionally, check that loss decreased over epochs
  expect_true(all(diff(history_sharpe_diff$metrics$loss) <= 0), info = "Loss did not consistently decrease for sharpe_ratio_difference_loss.")
})

test_that("information_ratio_loss_active_returns functions correctly", {
  # Define the configuration for information_ratio_loss_active_returns
  config_info_ratio_active <- list(
    layers = list(
      list(type = "dense", units = 32, activation = "relu"),
      list(type = "dense", units = 16, activation = "relu"),
      list(type = "dense", units = 1, activation = "linear")
    ),
    loss = list(
      name = "information_ratio_loss_active_returns",
      lambda_l1 = 0.01,
      lambda_l2 = 0.01
    ),
    optimizer = list(
      name = "optimizer_adam",
      learning_rate = 0.001
    ),
    metrics = list(
      distance_from_benchmark_l1_metric(),
      distance_from_benchmark_l2_metric()
    ),
    callbacks = list(
      callback_early_stopping(monitor = "loss", min_delta = 0.001, patience = 3)
    ),
    epochs = 10,
    batch_size = 128,
    verbose = 0,
    seeds = c(42),
    plot_training = FALSE,
    plot_result = FALSE
  )

  # Invoke keras_weights with the information_ratio_loss_active_returns config
  result_info_ratio_active <- tryCatch({
    keras_weights(train_data_ex, test_data_ex, config_info_ratio_active)
  }, error = function(e) {
    fail(paste("keras_weights failed with information_ratio_loss_active_returns:", e$message))
  })

  # Extract the training history
  history_info_ratio_active <- result_info_ratio_active$histories[[as.character(config_info_ratio_active$seeds[[1]])]]

  # Ensure that history_info_ratio_active is not NULL
  expect_true(!is.null(history_info_ratio_active), info = "History object is NULL for information_ratio_loss_active_returns.")

  # Extract final loss and metrics
  final_loss <- tail(history_info_ratio_active$metrics$loss, n = 1)
  final_l1 <- tail(history_info_ratio_active$metrics$distance_from_benchmark_l1, n = 1)
  final_l2 <- tail(history_info_ratio_active$metrics$distance_from_benchmark_l2, n = 1)

  # Assertions
  expect_true(is.finite(final_loss), info = "Final loss is not finite for information_ratio_loss_active_returns.")
  expect_true(is.finite(final_l1) && final_l1 >= 0, info = "Final L1 distance is not finite or negative for information_ratio_loss_active_returns.")
  expect_true(is.finite(final_l2) && final_l2 >= 0, info = "Final L2 distance is not finite or negative for information_ratio_loss_active_returns.")

  # Optionally, check that loss decreased over epochs
  expect_true(all(diff(history_info_ratio_active$metrics$loss) <= 0), info = "Loss did not consistently decrease for information_ratio_loss_active_returns.")
})

test_that("information_ratio_loss_regression_based functions correctly", {
  # Define the configuration for information_ratio_loss_regression_based
  config_info_ratio_regression <- list(
    layers = list(
      list(type = "dense", units = 32, activation = "relu"),
      list(type = "dense", units = 16, activation = "relu"),
      list(type = "dense", units = 1, activation = "linear")
    ),
    loss = list(
      name = "information_ratio_loss_regression_based",
      lambda_l1 = 0.01,
      lambda_l2 = 0.01
    ),
    optimizer = list(
      name = "optimizer_adam",
      learning_rate = 0.001
    ),
    metrics = list(
      distance_from_benchmark_l1_metric(),
      distance_from_benchmark_l2_metric()
    ),
    callbacks = list(
      callback_early_stopping(monitor = "loss", min_delta = 0.001, patience = 3)
    ),
    epochs = 10,
    batch_size = 128,
    verbose = 0,
    seeds = c(42),
    plot_training = FALSE,
    plot_result = FALSE
  )

  # Invoke keras_weights with the information_ratio_loss_regression_based config
  result_info_ratio_regression <- tryCatch({
    keras_weights(train_data_ex, test_data_ex, config_info_ratio_regression)
  }, error = function(e) {
    fail(paste("keras_weights failed with information_ratio_loss_regression_based:", e$message))
  })

  # Extract the training history
  history_info_ratio_regression <- result_info_ratio_regression$histories[[as.character(config_info_ratio_regression$seeds[[1]])]]

  # Ensure that history_info_ratio_regression is not NULL
  expect_true(!is.null(history_info_ratio_regression), info = "History object is NULL for information_ratio_loss_regression_based.")

  # Extract final loss and metrics
  final_loss <- tail(history_info_ratio_regression$metrics$loss, n = 1)
  final_l1 <- tail(history_info_ratio_regression$metrics$distance_from_benchmark_l1, n = 1)
  final_l2 <- tail(history_info_ratio_regression$metrics$distance_from_benchmark_l2, n = 1)

  # Assertions
  expect_true(is.finite(final_loss), info = "Final loss is not finite for information_ratio_loss_regression_based.")
  expect_true(is.finite(final_l1) && final_l1 >= 0, info = "Final L1 distance is not finite or negative for information_ratio_loss_regression_based.")
  expect_true(is.finite(final_l2) && final_l2 >= 0, info = "Final L2 distance is not finite or negative for information_ratio_loss_regression_based.")

  # Optionally, check that loss decreased over epochs
  expect_true(all(diff(history_info_ratio_regression$metrics$loss) <= 0), info = "Loss did not consistently decrease for information_ratio_loss_regression_based.")
})

