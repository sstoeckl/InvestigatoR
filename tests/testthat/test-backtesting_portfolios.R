library(testthat)
library(dplyr)
library(tibble)

# Create a small subset of data_ml for testing
test_data_ml <- data_ml %>%
  filter(stock_id <= 5)

return_label <- "R1M_Usd"
features <- c("Div_Yld", "Eps", "Mkt_Cap_12M_Usd", "Mom_11M_Usd", "Ocf", "Pb", "Vol1Y_Usd")

# Create a dummy weight function for testing
dummy_weights_func <- function(train_data, test_data, config) {
  tibble::tibble(
    stock_id = test_data$stock_id,
    date = test_data$date,
    pred_weight = runif(nrow(test_data), -1, 1)  # Random weights for example
  )
}

# pf_config for testing backtesting_portfolios
pf_config <- list(
  predictions = c("dummy_weights_func"),
  dummy_weights_func = list(list())
)


rolling <- FALSE
window_size <- "5 years"
step_size <- "1 year"
offset <- "1 month"
in_sample <- TRUE
num_cores <- 2

# Test 1: Create portfolioReturns object using create_portfolios
test_that("create_portfolios function works correctly", {
  portfolio <- create_portfolios(test_data_ml, return_label)

  # Check if the portfolioReturns object is created correctly
  expect_s3_class(portfolio, "portfolioReturns")
  expect_true("models" %in% names(portfolio))
  expect_true("weights" %in% names(portfolio))
  expect_true("actual_returns" %in% names(portfolio))
  expect_true("portfolio_returns" %in% names(portfolio))

  # Check that actual_returns contains correct stock_id and date columns
  expect_true(all(c("stock_id", "date", "actual_return") %in% colnames(portfolio$actual_returns)))

  # Check for correct number of rows in weights and actual_returns
  expect_equal(nrow(portfolio$weights), nrow(test_data_ml))
  expect_equal(nrow(portfolio$actual_returns), nrow(test_data_ml))
})

# Test 2: Add weight model to portfolioReturns using add_weight_model
test_that("add_weight_model adds a new weight model correctly", {
  portfolio <- create_portfolios(test_data_ml, return_label)

  new_weights <- dummy_weights_func(train_data = NULL, test_data = test_data_ml, config = NULL)

  portfolio <- add_weight_model(portfolio, model_name = "dummy_weights_func", weight_config = list(), new_weights = new_weights)

  # Check if the weight model has been added correctly
  expect_true("weights" %in% names(portfolio))
  expect_true("dummy_weights_func_1" %in% colnames(portfolio$weights))

  # Check that the portfolio_returns has been updated
  expect_true("dummy_weights_func_1" %in% colnames(portfolio$portfolio_returns))

  # Check that portfolio_returns and weights have the correct number of rows
  expect_equal(nrow(portfolio$weights), nrow(test_data_ml))
  expect_equal(nrow(portfolio$portfolio_returns), length(unique(test_data_ml$date)))
})

# Test for backtesting_weights
test_that("backtesting_weights function works correctly with dummy_weights_func", {
  result <- backtesting_weights(
    data = test_data_ml,
    return_label = return_label,
    features = features,
    pf_config = pf_config,
    rolling = rolling,
    window_size = window_size,
    step_size = step_size,
    offset = offset,
    in_sample = in_sample,
    verbose = FALSE
  )

  # Check that the result is a valid portfolioReturns object
  expect_s3_class(result, "portfolioReturns")

  # Check that weight_models, weights, and portfolio_returns are present in the portfolio
  expect_true("models" %in% names(result))
  expect_true("weights" %in% names(result))
  expect_true("portfolio_returns" %in% names(result))

  # Check for the correct number of rows in the weights and portfolio_returns
  expect_equal(nrow(result$weights), nrow(test_data_ml))
  expect_equal(nrow(result$portfolio_returns), length(unique(test_data_ml$date)))

  # Check that the weight model dummy_weights_func_1 exists
  expect_true("dummy_weights_func_1" %in% colnames(result$weights))
  expect_true("dummy_weights_func_1" %in% colnames(result$portfolio_returns))
})
