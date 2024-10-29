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

test_that("create_portfolioReturns creates object correctly without benchmark", {

  # Create portfolioReturns object
  pf <- create_portfolioReturns(test_data_ml, return_label)

  # Check class
  expect_s3_class(pf, "portfolioReturns")

  # Check structure
  expect_true(all(c("models", "postprocessing_config", "weights", "delta_weights",
                    "benchmark_weights", "actual_returns", "portfolio_returns",
                    "benchmark_returns") %in% names(pf)))

  # Check weights
  expect_equal(nrow(pf$weights), nrow(test_data_ml))
  expect_true(all(c("stock_id", "date") %in% names(pf$weights)))

  # Check delta_weights and benchmark_weights are NULL
  expect_null(pf$delta_weights)
  expect_null(pf$benchmark_weights)

  # Check actual_returns
  expect_equal(nrow(pf$actual_returns), nrow(test_data_ml))
  expect_true(all(c("stock_id", "date", "actual_return") %in% names(pf$actual_returns)))

  # Check portfolio_returns
  expect_equal(nrow(pf$portfolio_returns), length(unique(test_data_ml$date)))
  expect_true("date" %in% names(pf$portfolio_returns))

  # Check no benchmark_returns
  expect_null(pf$benchmark_returns)
})

test_that("create_portfolioReturns creates object correctly with benchmark", {
  # Sample data with benchmark
  test_data_ml2 <- test_data_ml %>% mutate(benchmark=runif(nrow(test_data_ml)))

  # Create portfolioReturns object with benchmark
  pf <- create_portfolioReturns(test_data_ml2, return_label, "benchmark")

  # Check class
  expect_s3_class(pf, "portfolioReturns")

  # Check structure
  expect_true(all(c("models", "postprocessing_config", "weights", "delta_weights",
                    "benchmark_weights", "actual_returns", "portfolio_returns",
                    "benchmark_returns") %in% names(pf)))

  # Check weights
  expect_equal(nrow(pf$weights), nrow(test_data_ml2))
  expect_true(all(c("stock_id", "date") %in% names(pf$weights)))

  # Check delta_weights
  expect_equal(nrow(pf$delta_weights), nrow(pf$weights))
  expect_true(all(c("stock_id", "date") %in% names(pf$delta_weights)))

  # Check benchmark_weights
  expect_equal(nrow(pf$benchmark_weights), nrow(test_data_ml2))
  expect_true(all(c("stock_id", "date", "benchmark_weight") %in% names(pf$benchmark_weights)))

  # Check actual_returns
  expect_equal(nrow(pf$actual_returns), nrow(test_data_ml2))
  expect_true(all(c("stock_id", "date", "actual_return") %in% names(pf$actual_returns)))

  # Check portfolio_returns
  expect_equal(nrow(pf$portfolio_returns), length(unique(test_data_ml2$date)))
  expect_true("date" %in% names(pf$portfolio_returns))
  expect_true("benchmark_return" %in% names(pf$benchmark_returns))
})

test_that("create_portfolioReturns handles empty data gracefully", {
  # Empty data frame
  data_empty <- tibble(
    stock_id = integer(),
    date = as.Date(character()),
    return_label = numeric()
  )

  pf <- create_portfolioReturns(data_empty, "return_label")

  # Check class
  expect_s3_class(pf, "portfolioReturns")

  # Check components are empty
  expect_equal(nrow(pf$weights), 0)
  expect_null(pf$delta_weights)
  expect_null(pf$benchmark_weights)
  expect_equal(nrow(pf$actual_returns), 0)
  expect_equal(nrow(pf$portfolio_returns), 0)
  expect_null(pf$benchmark_returns)
})

####################################################################################################

test_that("add_weight_model adds model correctly to non-benchmark portfolio", {
  pf <- create_portfolioReturns(test_data_ml, return_label)

  # New weights
  new_weights <- test_data_ml %>%
    select(stock_id,date) %>%  mutate(weight=runif(nrow(test_data_ml), -1, 1)) %>%
    arrange(stock_id,date)

  # Add weight model
  pf_updated <- add_weight_model(
    portfolio_object = pf,
    model_name = "Model_A",
    new_weights = new_weights,
    config=list()
  )

  # Check that model is added
  expect_true("Model_A_1" %in% names(pf_updated$models))

  # Check weights updated
  expect_true("Model_A_1" %in% names(pf_updated$weights))
  expect_equal(pf_updated$weights$Model_A_1, new_weights$weight)

  # Check portfolio_returns updated
  expected_returns <- pf_updated$weights %>%
    inner_join(pf_updated$actual_returns, by = c("stock_id", "date")) %>%
    group_by(date) %>%
    summarize(portfolio_return = sum(Model_A_1 * actual_return, na.rm = TRUE)) %>%
    arrange(date) %>%
    pull(portfolio_return)

  model_returns <- pf_updated$portfolio_returns %>%
    pull("Model_A_1")

  expect_equal(model_returns, expected_returns)
})

test_that("add_weight_model adds model correctly to benchmark portfolio", {
  # Sample data with benchmark
  test_data_ml2 <- test_data_ml %>% mutate(benchmark=runif(nrow(test_data_ml)))

  # Create portfolioReturns object with benchmark
  pf <- create_portfolioReturns(test_data_ml2, return_label, "benchmark")
  # New delta weights
  new_weights <- test_data_ml %>%
    select(stock_id,date) %>%  mutate(weight=runif(nrow(test_data_ml), -0.1, 0.1)) %>%
    arrange(stock_id, date)

  # Benchmark weights
  benchmark_weights <- pf$benchmark_weights

  # Add weight model
  pf_updated <- add_weight_model(
    portfolio_object = pf,
    model_name = "Model_B",
    new_weights = new_weights,
    config=list()
  )

  # Check that model is added
  expect_true("Model_B_1" %in% names(pf_updated$models))

  # Check delta_weights updated
  expect_true("Model_B_1" %in% names(pf_updated$delta_weights))
  expect_equal(pf_updated$delta_weights$Model_B_1, new_weights$weight)

  # Check weights = benchmark_weight + delta_weight
  expected_weights <- pf_updated$benchmark_weights %>%
    inner_join(pf_updated$delta_weights, by = c("stock_id", "date")) %>%
    mutate(weight = benchmark_weight + Model_B_1) %>%
    select(stock_id, date, weight) %>%
    arrange(stock_id,date)

  expect_equal(pf_updated$weights$Model_B_1, expected_weights$weight)

  # Check portfolio_returns updated
  expected_returns <- pf_updated$weights %>%
    inner_join(pf$actual_returns, by = c("stock_id", "date")) %>%
    group_by(date) %>%
    summarize(portfolio_return = sum(Model_B_1 * actual_return, na.rm = TRUE)) %>%
    arrange(date) %>%
    pull(portfolio_return)

  model_returns <- pf_updated$portfolio_returns %>%
    pull("Model_B_1")

  expect_equal(model_returns, expected_returns)
})


###################################################################################
# tests/testthat/test_postprocessing_portfolios.R

# Helper function to create a portfolioReturns object with a weight model
test_that("postprocessing_portfolios applies set_weightsum correctly to non-benchmark portfolio", {
  pf <- create_portfolioReturns(test_data_ml, return_label)

  # New weights
  new_weights <- test_data_ml %>%
    select(stock_id,date) %>%  mutate(weight=runif(nrow(test_data_ml), -1, 1)) %>%
    arrange(date)

  # Add weight model
  pf_updated <- add_weight_model(
    portfolio_object = pf,
    model_name = "Model_A",
    new_weights = new_weights,
    config = list("keras_super",config=list("layer"="deep"))
  )

  config <- list(
    list(
      operation = "set_weightsum",
      sum = 1,
      allow_short_sale = FALSE
    )
  )

  pf_processed <- postprocessing_portfolios(pf_updated, config)

  # Check that weights sum to 1 per date
  weights_sum <- pf_processed$weights %>%
    select(1,2,weight=3) %>%
    group_by(date) %>%
    summarize(total = sum(weight, na.rm = TRUE)) %>%
    pull(total)

  expect_true(all(abs(weights_sum - 1) < 1e-6))

  # Check no negative weights
  expect_true(all(pull(pf_processed$weights,3) >= 0))
})

test_that("postprocessing_portfolios applies flatten_weights correctly", {
  pf <- create_portfolioReturns(test_data_ml, return_label)

  # New weights
  new_weights <- test_data_ml %>%
    select(stock_id,date) %>%  mutate(weight=runif(nrow(test_data_ml), -1, 1)) %>%
    arrange(date)

  # Add weight model
  pf_updated <- add_weight_model(
    portfolio_object = pf,
    model_name = "Model_A",
    new_weights = new_weights,
    config=list()
  )

  config <- list(
    list(
      operation = "flatten_weights",
      l1 = 0,
      l2 = 0,
      mix = 0
    )
  )

  pf_processed <- postprocessing_portfolios(pf_updated, config)

  # Check that weights have been reduced
  original_weights <- pull(pf_updated$weights,3)
  processed_weights <- pull(pf_processed$weights,3)
  # original_weights - processed_weights
  expect_true(all(processed_weights <= original_weights + 1e-6))  # Allow small numerical differences
})

test_that("postprocessing_portfolios applies reduce_turnover correctly", {
  pf <- create_portfolioReturns(test_data_ml, return_label)

  # New weights
  new_weights <- test_data_ml %>%
    select(stock_id,date) %>%  mutate(weight=runif(nrow(test_data_ml), -1, 1)) %>%
    arrange(date)

  # Add weight model
  pf_updated <- add_weight_model(
    portfolio_object = pf,
    model_name = "Model_A",
    new_weights = new_weights,
    config=list()
  )

  # Simulate previous weights by adding another model
  previous_weights <- pf_updated$weights

  config <- list(
    list(
      operation = "reduce_turnover",
      method = "linear",
      smoothing_factor = 0.2
    )
  )

  pf_processed <- postprocessing_portfolios(pf_updated, config)

  # Check that weights have been smoothed
  # Since we have only two dates, previous weights are assumed to be 'Model_prev'
  # and current weights are the main 'weight' column.

  expected_weights <- pf_updated$weights %>%
    select(1,2,weight=3) %>%
    arrange(stock_id, date) %>%
    group_by(stock_id) %>%
    mutate(weight_prev = dplyr::lag(weight, default = weight[1])) %>%
    mutate(weight_expected = weight_prev + (weight - weight_prev) * (1 - 0.2)) %>%
    ungroup() %>%
    pull(weight_expected)

  expect_equal(round(pull(pf_processed$weights,3), 6), round(expected_weights, 6))
})

test_that("postprocessing_portfolios applies increase_diversification correctly", {
  pf <- create_portfolioReturns(test_data_ml, return_label)

  # New weights
  new_weights <- test_data_ml %>%
    select(stock_id,date) %>%  mutate(weight=ifelse(stock_id==1,0.8,0.1)) %>%
    arrange(date)

  # Add weight model
  pf_updated <- add_weight_model(
    portfolio_object = pf,
    model_name = "Model_A",
    new_weights = new_weights,
    config=list()
  )

  config <- list(
    list(
      operation = "increase_diversification",
      hh_target = 0.3
    )
  )

  pf_processed <- postprocessing_portfolios(pf_updated, config)

  # Calculate new HHI per date
  hhi_new <- pf_processed$weights %>%
    select(1,2,weight=3) %>%
    group_by(date) %>%
    summarize(hhi = sum(weight^2, na.rm = TRUE)) %>%
    pull(hhi)

  expect_true(all(hhi_new <= 0.3 + 1e-6))
})

test_that("postprocessing_portfolios handles multiple operations correctly", {
  pf <- create_portfolioReturns(test_data_ml, return_label)

  # New weights
  new_weights <- test_data_ml %>%
    select(stock_id,date) %>%  mutate(weight=ifelse(stock_id==1,0.8,0.1)) %>%
    arrange(date)

  # Add weight model
  pf_updated <- add_weight_model(
    portfolio_object = pf,
    model_name = "Model_A",
    new_weights = new_weights,
    config=list()
  )

  config <- list(
    list(
      operation = "set_weightsum",
      sum = 1,
      allow_short_sale = FALSE
    ),
    list(
      operation = "flatten_weights",
      l1 = 0.02,
      l2 = 0.05,
      mix = 0.5
    ),
    list(
      operation = "reduce_turnover",
      method = "exponential",
      smoothing_factor = 0.3
    ),
    list(
      operation = "increase_diversification",
      hh_target = 0.3
    ),
    list(
      operation = "set_weightsum",
      sum = 1,
      allow_short_sale = FALSE
    )
  )

  pf_processed <- postprocessing_portfolios(pf_updated, config)

  # Check weights sum to 1 per date
  weights_sum <- pf_processed$weights %>%
    select(1,2,weight=3) %>%
    group_by(date) %>%
    summarize(total = sum(weight, na.rm = TRUE)) %>%
    pull(total)

  expect_true(all(abs(weights_sum - 1) < 1e-6))

  # Check no negative weights
  expect_true(all(pull(pf_processed$weights,3) >= 0))

  # Check HHI meets target
  hhi_new <- pf_processed$weights %>%
    select(1,2,weight=3) %>%
    group_by(date) %>%
    summarize(hhi = sum(weight^2, na.rm = TRUE)) %>%
    pull(hhi)

  expect_true(all(hhi_new <= 0.66 + 1e-6))
})

test_that("postprocessing_portfolios handles benchmark portfolio correctly", {
  # Sample data with benchmark
  test_data_ml2 <- test_data_ml %>% mutate(benchmark=runif(nrow(test_data_ml)))

  # Create portfolioReturns object with benchmark
  pf <- create_portfolioReturns(test_data_ml2, return_label, "benchmark")
  # New delta weights
  new_weights <- test_data_ml %>%
    select(stock_id,date) %>%  mutate(weight=runif(nrow(test_data_ml), -0.1, 0.1)) %>%
    arrange(stock_id, date)

  # Benchmark weights
  benchmark_weights <- pf$benchmark_weights

  # Add weight model
  pf_updated <- add_weight_model(
    portfolio_object = pf,
    model_name = "Model_B",
    new_weights = new_weights,
    config=list()
  )

  config <- list(
    list(
      operation = "flatten_weights",
      l1 = 0.02,
      l2 = 0.05,
      mix = 0.5
    ),
    list(
      operation = "set_weightsum",
      sum = 0,
      min_sum = 0,
      max_sum = 1,
      min_weight = -0.1,
      max_weight = 0.1,
      allow_short_sale = TRUE
    )
  )

  pf_processed <- postprocessing_portfolios(pf_updated, config)

  # Check that delta_weights are updated
  expect_true("Model_B_1" %in% names(pf_processed$delta_weights))

  # Check that weights sum to 1 per date
  weights_sum <- pf_processed$delta_weights %>%
    select(1,2,weight=3) %>%
    group_by(date) %>%
    summarize(total = sum(weight, na.rm = TRUE)) %>%
    pull(total)

  expect_true(all(abs(weights_sum) < 1e-1))

  # Check no weights<>1
  expect_true(all(pull(pf_processed$delta_weights,3) < 1))

  # Check portfolio_returns are recalculated
  expected_returns <- pf_processed$weights %>%
    select(1,2,weight=3) %>%
    inner_join(pf$actual_returns, by = c("date", "stock_id")) %>%
    group_by(date) %>%
    summarize(portfolio_return = sum(weight * actual_return, na.rm = TRUE)) %>%
    arrange(date) %>%
    pull(portfolio_return)

  model_returns <- pf_processed$portfolio_returns %>%
    pull(2)

  expect_equal(model_returns, expected_returns)
})

