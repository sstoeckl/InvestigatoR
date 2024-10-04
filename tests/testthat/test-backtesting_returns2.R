# tests/testthat/test-backtesting_returns.R

library(testthat)
library(tidyverse)
library(reticulate)
library(tensorflow)
library(keras)
reticulate::use_virtualenv("C:/R/python/")
reticulate::py_config()

# Load the data_ml dataset
data("data_ml")

# Create a small subset of data_ml for testing
test_data_ml <- data_ml %>%
  filter(stock_id<=5)

# Define common parameters for testing
return_label <- "R1M_Usd"
features <- c("Div_Yld", "Eps", "Mkt_Cap_12M_Usd", "Mom_11M_Usd", "Ocf", "Pb", "Vol1Y_Usd")
rolling <- FALSE
window_size <- "5 years"
step_size <- "1 month"
offset <- "1 month"
in_sample <- TRUE

test_that("keras_pred works correctly", {

  # Split the test data into training and testing sets
  train_data_ex <- test_data_ml[1:150, c("stock_id", "date", return_label, features)]
  test_data_ex <- test_data_ml[151:200, c("stock_id", "date", features)]

  # Define model configuration
  config <- list(
    layers = list(
      list(type = "dense", units = 32, activation = "relu"),
      list(type = "dense", units = 16, activation = "relu"),
      list(type = "dense", units = 1, activation = "linear")  # Single output for regression
    ),
    loss = 'mean_squared_error',
    optimizer = list(name = "optimizer_rmsprop", learning_rate = 0.001),
    epochs = 10,
    batch_size = 32
  )

  # Run keras_pred function
  predictions <- keras_pred(train_data, test_data, config)

  # Test if the predictions output is a tibble
  expect_s3_class(predictions,"tbl_df")
  expect_true(all(c("stock_id", "date", "pred_return") %in% colnames(predictions)))
  expect_equal(nrow(predictions), nrow(test_data_ex))

})

# Define Keras-based ML configurations for testing
ml_config_keras <- list(
  keras_pred_1 = list(
    pred_func = "keras_pred",
    config = list(
      layers = list(
        list(type = "dense", units = 32, activation = "relu"),
        list(type = "dense", units = 16, activation = "relu"),
        list(type = "dense", units = 1, activation = "linear")
      ),
      loss = 'mean_squared_error',
      optimizer = list(name = "optimizer_rmsprop", learning_rate = 0.001),
      epochs = 10,
      batch_size = 32
    )
  ),
  keras_pred_2 = list(
    pred_func = "keras_pred",
    config = list(
      layers = list(
        list(type = "dense", units = 64, activation = "relu"),
        list(type = "dense", units = 32, activation = "relu"),
        list(type = "dense", units = 1, activation = "linear")
      ),
      loss = 'mean_squared_error',
      optimizer = list(name = "optimizer_adam", learning_rate = 0.001),
      epochs = 15,
      batch_size = 16
    )
  ),
  keras_pred_3 = list(
    pred_func = "keras_pred",
    config = list(
      layers = list(
        list(type = "dense", units = 16, activation = "tanh"),
        list(type = "dense", units = 8, activation = "elu"),
        list(type = "dense", units = 1, activation = "linear")
      ),
      loss = 'mean_squared_error',
      optimizer = list(name = "optimizer_sgd", learning_rate = 0.01),
      epochs = 5,
      batch_size = 32
    )
  )
)

# Test for backtesting_returns with Keras models
test_that("backtesting_returns executes correctly with multiple Keras models", {
  # Run backtesting_returns with multiple Keras models
  rp_keras <- backtesting_returns(
    data = test_data_ml,
    return_prediction_object = NULL,
    return_label = return_label,
    features = features,
    rolling = rolling,
    window_size = window_size,
    step_size = step_size,
    offset = offset,
    in_sample = in_sample,
    ml_config = ml_config_keras,
    append = FALSE,
    num_cores = 1L,  # Use single core for testing
    verbose = TRUE  # Set to TRUE for detailed messages during testing
  )

  # Verify the returned object
  expect_s3_class(rp_keras, "returnPrediction")

  # Verify the number of models is correct
  expect_true(length(rp_keras$models) == length(ml_config_keras))

  # Verify predictions
  expect_true(nrow(rp_keras$predictions) > 0)
  expect_true(all(c("stock_id", "date") %in% colnames(rp_keras$predictions)))

  # Verify the summary of predictions
  summary_keras <- summary(rp_keras)
  expect_true(all(colnames(summary_keras) == c("MSE", "RMSE", "MAE", "Hit_Ratio")))

  # Verify rownames in summary match model identifiers
  expect_true(all(rownames(summary_keras) == colnames(rp_keras$predictions)[-c(1:2)]))
})



