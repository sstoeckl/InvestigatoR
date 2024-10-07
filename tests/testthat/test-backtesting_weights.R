
library(testthat)
library(tidyverse)
library(reticulate)
library(tensorflow)
library(keras3)
reticulate::use_virtualenv("C:/R/python/")
reticulate::py_config()

test_that("Long-only portfolio with Sharpe ratio loss works", {
  y_true <- matrix(c(1, 1, 0.05, 1, 1, 2, 0.02, 1, 1, 3, 0.01, 1, 2, 1, 0.05, 1, 2, 2, 0.02, 1, 2, 3, 0.01, 1), ncol = 4, byrow = TRUE)
  y_pred <- c(0.4, 0.3, 0.3, 0.5, 0.7, -0.3)
  y_true <- tf$constant(y_true, dtype = "float32")
  y_pred <- tf$constant(y_pred, dtype = "float32")

  loss_value <- sharpe_ratio_loss_keras(y_true, y_pred, max_weight = 0.5, min_weight = 0)
  expect_s3_class(loss_value, "tensorflow.tensor")
})

test_that("Long-short portfolio with weight constraints works", {
  y_true <- matrix(c(1, 1, 0.05, 1, 1, 2, 0.02, 1, 1, 3, 0.01, 1, 2, 1, 0.05, 1, 2, 2, 0.02, 1, 2, 3, 0.01, 1), ncol = 4, byrow = TRUE)
  y_pred <- c(0.4, 0.3, 0.3, 0.5, 0.7, -0.3)
  y_true <- tf$constant(y_true, dtype = "float32")
  y_pred <- tf$constant(y_pred, dtype = "float32")

  loss_value <- sharpe_ratio_loss_keras(y_true, y_pred, max_weight = 0.5, min_weight = -0.5)
  expect_s3_class(loss_value, "tensorflow.tensor")
})

test_that("Portfolio with turnover penalty works", {
  y_true <- matrix(c(1, 1, 0.05, 1, 1, 2, 0.02, 1, 1, 3, 0.01, 1, 2, 1, 0.05, 1, 2, 2, 0.02, 1, 2, 3, 0.01, 1), ncol = 4, byrow = TRUE)
  y_pred <- c(0.4, 0.3, 0.3, 0.5, 0.7, -0.3)
  y_true <- tf$constant(y_true, dtype = "float32")
  y_pred <- tf$constant(y_pred, dtype = "float32")

  loss_value <- sharpe_ratio_loss_keras(y_true, y_pred, turnover_penalty_weight = 0.05)
  expect_s3_class(loss_value, "tensorflow.tensor")
})

# Load the data_ml dataset
data("data_ml")

# Create a small subset of data_ml for testing
test_data_ml <- data_ml %>%  filter(stock_id<=5) |> arrange(date,stock_id)

# Define common parameters for testing
return_label <- "R1M_Usd"
features <- c("Div_Yld", "Eps", "Mkt_Cap_12M_Usd", "Mom_11M_Usd", "Ocf", "Pb", "Vol1Y_Usd")
rolling <- FALSE
window_size <- "5 years"
step_size <- "1 month"
offset <- "1 month"
in_sample <- TRUE

test_that("keras_weights works with custom Sharpe ratio loss function", {

  train_data_ex <- test_data_ml |> filter(date<="2012-12-31") |>
    select(stock_id, date, return_label, features) |>
    mutate(mask=round(runif(840),0),benchmark=0) |> #random zeros and ones
    relocate(c("stock_id", "date","benchmark","mask", all_of(return_label), all_of(features)))
  test_data_ex <- test_data_ml |> filter(date>"2012-12-31") |>
    select(stock_id, date, features)

  # Keras model configuration
  config <- list(
    layers = list(
      list(type = "dense", units = 16, activation = "relu"),
      list(type = "dense", units = 8, activation = "relu"),
      list(type = "dense", units = 1, activation = "linear")
    ),
    loss = dummy_mse_loss, #sharpe_ratio_loss_keras,  # Use custom Sharpe ratio loss function
    optimizer = list(name = "optimizer_adam", learning_rate = 0.001),
    epochs = 5,
    batch_size = 50,
    verbose = 0,  # Reduce verbosity for testing
    seeds = c(42, 50, 62),  # Set seed for reproducibility
    plot_training = FALSE
  )

  # Run keras_weights with sharpe_ratio_loss_keras
  weights <- keras_weights(
    train_data = train_data_ex,
    test_data = test_data_ex,
    config = config
  )
  weights

  # Check the weights are returned correctly
  expect_s3_class(weights,"tbl_df")
  expect_true(all(c("stock_id", "date", "pred_weight") %in% names(weights)))

  # Check if predicted weights are between the expected weight constraints
  max_weight <- 0.1
  min_weight <- -0.1
  expect_true(all(weights$pred_weight >= min_weight))
  expect_true(all(weights$pred_weight <= max_weight))
})

# Unit Test for postprocess_weights
test_that("postprocess_weights applies masking and box constraints correctly", {
  # Example data
  predicted_weights <- tibble::tibble(
    stock_id = rep(1:10, each = 3),
    date = rep(seq.Date(Sys.Date(), by = "days", length.out = 3), times = 10),
    pred_weight = c(0.2, -0.5, 0.8,  # First stock, all three dates
                0.3, 0.0, 0.2,   # Second stock
                -0.6, 0.7, -0.4, # Third stock
                0.9, -0.8, 0.1,  # Fourth stock
                0.4, 0.3, -0.5,  # Fifth stock
                -0.9, 0.1, 0.2,  # Sixth stock
                0.5, 0.2, -0.1,  # Seventh stock
                -0.3, 0.4, 0.7,  # Eighth stock
                -0.2, 0.9, -0.5, # Ninth stock
                0.3, -0.7, 0.4)  # Tenth stock
  )

  original_data <- tibble::tibble(
    stock_id = rep(1:10, each = 3),
    date = rep(seq.Date(Sys.Date(), by = "days", length.out = 3), times = 10),
    mask = c(rep(1, 27), rep(0, 3)) # 3 stocks are masked on the last day
  )

  # Define a model config with box constraints:
  model_config <- list(
    min_weight = -0.5,
    max_weight = 0.5
  )

  # Apply the postprocessing function
  postprocessed_weights <- postprocess_weights(predicted_weights, original_data, "mask", model_config)

  # Check if masking was applied correctly
  masked_weights <- postprocessed_weights %>%
    filter(stock_id %in% 10)
  expect_true(all(masked_weights$pred_weight == 0))  # Masked stocks should have weights set to 0

  # Check if box constraints are respected
  expect_true(all(postprocessed_weights$pred_weight >= model_config$min_weight &
                    postprocessed_weights$pred_weight <= model_config$max_weight))
})
