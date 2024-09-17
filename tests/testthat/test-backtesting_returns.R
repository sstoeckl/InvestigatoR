# tests/testthat/test-backtesting_returns.R

test_that("backtesting_returns produces expected predictions", {
  # Load sample data
  data_ml_subset <- tibble::tibble(
    stock_id = rep(1:3, each = 4),
    date = rep(seq.Date(from = as.Date("2020-01-01"), by = "month", length.out = 4), times = 3),
    R1M_Usd = c(0.05, 0.02, -0.03, 0.04, 0.01, -0.02, 0.03, 0.05, -0.01, 0.02, 0.04, 0.03),
    Div_Yld = runif(12, 1, 5),
    Eps = runif(12, 0.5, 2),
    Mkt_Cap_12M_Usd = runif(12, 100, 500),
    Mom_11M_Usd = runif(12, -0.1, 0.1),
    Ocf = runif(12, 10, 50),
    Pb = runif(12, 0.5, 3),
    Vol1Y_Usd = runif(12, 1000, 5000)
  )

  # Define indices helper function (mock)
  select_dates_by_offset <- function(dates, window_size, step_size, offset, rolling) {
    # Simple mock implementation for testing
    tibble::tibble(
      training_start = as.Date("2020-01-01"),
      training_end = as.Date("2020-04-01"),
      prediction_start = as.Date("2020-05-01"),
      prediction_end = as.Date("2020-05-01")
    )
  }

  # Define create_return_prediction helper function (mock)
  create_return_prediction <- function(data_subset, return_label) {
    list(
      models = list(),
      predictions = data_subset %>%
        select(stock_id, date, !!return_label),
      actual_returns = data_subset %>%
        select(stock_id, date, !!return_label),
      errors = NULL
    )
  }

  # Define add_model_prediction helper function (mock)
  add_model_prediction <- function(return_prediction_object, pred_func, config, predictions) {
    return_prediction_object$predictions <- predictions
    return(return_prediction_object)
  }

  # Define a simple prediction function
  simple_pred_func <- function(train_data, test_data, config) {
    tibble::tibble(
      stock_id = test_data$stock_id,
      date = test_data$date,
      pred_return = mean(train_data$R1M_Usd)
    )
  }

  # Assign to global environment for testing
  assign("select_dates_by_offset", select_dates_by_offset, envir = .GlobalEnv)
  assign("create_return_prediction", create_return_prediction, envir = .GlobalEnv)
  assign("add_model_prediction", add_model_prediction, envir = .GlobalEnv)
  assign("simple_pred_func", simple_pred_func, envir = .GlobalEnv)

  # Define ml_config
  ml_config <- list(
    simple_model = list(pred_func = "simple_pred_func")
  )

  # Call backtesting_returns
  rp <- backtesting_returns(
    data = data_ml_subset,
    return_prediction_object = NULL,
    return_label = "R1M_Usd",
    features = c("Div_Yld", "Eps", "Mkt_Cap_12M_Usd", "Mom_11M_Usd", "Ocf", "Pb", "Vol1Y_Usd"),
    rolling = FALSE,
    window_size = "4 months",
    step_size = "1 month",
    offset = "0 months",
    in_sample = FALSE,
    ml_config = ml_config,
    append = FALSE,
    num_cores = 1,
    verbose = FALSE
  )

  # Expected predictions: mean of R1M_Usd from training period
  expected_mean <- mean(data_ml_subset$R1M_Usd[data_ml_subset$date <= as.Date("2020-04-01")])

  # Check predictions
  expected_predictions <- tibble::tibble(
    stock_id = c(1, 2, 3),
    date = as.Date(c("2020-05-01", "2020-05-01", "2020-05-01")),
    prediction = rep(expected_mean, 3)
  )

  expect_equal(rp$predictions, expected_predictions)

  # Clean up
  rm("select_dates_by_offset", "create_return_prediction", "add_model_prediction", "simple_pred_func", envir = .GlobalEnv)
})
