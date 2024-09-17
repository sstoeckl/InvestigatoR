# tests/testthat/test-backtesting_portfolios.R

test_that("backtesting_portfolios adds weights correctly", {
  # Load sample return_prediction_object

  return_prediction_object <- list(
    models = list(),
    predictions = tibble::tibble(
      stock_id = rep(1:3, each = 2),
      date = rep(as.Date(c("2020-05-01", "2020-06-01")), times = 3),
      ols_pred = c(0.1, 0.2, 0.3, -0.1, 0.5, -0.2)
    ),
    actual_returns = tibble::tibble(
      stock_id = rep(1:3, each = 2),
      date = rep(as.Date(c("2020-05-01", "2020-06-01")), times = 3),
      R1M_Usd = c(0.05, 0.02, -0.03, 0.04, 0.01, -0.02)
    ),
    errors = NULL
  )

  class(return_prediction_object) <- "returnPrediction"

  # Define portfolio creation helper functions (mock)
  create_portfolios <- function(actual_returns, return_label) {
    list(
      portfolios = tibble::tibble(
        stock_id = actual_returns$stock_id,
        date = actual_returns$date,
        actual_return = actual_returns[[return_label]]
      )
    )
  }

  add_weight_model <- function(portfolio_object, weight_model_name, config, new_weights) {
    portfolio_object$portfolios <- portfolio_object$portfolios %>%
      left_join(new_weights, by = c("stock_id", "date"))
    return(portfolio_object)
  }

  # Assign helper functions to global environment
  assign("create_portfolios", create_portfolios, envir = .GlobalEnv)
  assign("add_weight_model", add_weight_model, envir = .GlobalEnv)

  # Define a simple weight function
  simple_weight_func <- function(return_predictions, errors, constraints = NULL) {
    # For testing, assign equal weights
    tibble::tibble(
      stock_id = return_predictions$stock_id,
      date = return_predictions$date,
      weight = 1 / nrow(return_predictions)
    )
  }

  # Assign to global environment
  assign("simple_weight_func", simple_weight_func, envir = .GlobalEnv)

  # Define pf_config
  pf_config <- list(
    equal_weight = list(
      pred_func = "simple_weight_func",
      config1 = list()
    )
  )

  # Call backtesting_portfolios
  portfolio_object <- backtesting_portfolios(
    return_prediction_object = return_prediction_object,
    portfolio_object = NULL,
    pf_config = pf_config,
    append = FALSE,
    verbose = FALSE
  )

  # Expected weights: 1/3 for each stock (since nrow(return_predictions) = 6 but per date, check accordingly)
  expected_weights <- tibble::tibble(
    stock_id = rep(1:3, each = 2),
    date = rep(as.Date(c("2020-05-01", "2020-06-01")), times = 3),
    weight = rep(1/3, 6),
    weight_model = rep("equal_weight", 6),
    config = rep("config1", 6)
  )

  # Check that weights are added correctly
  expect_equal(portfolio_object$portfolios,
               tibble::tibble(
                 stock_id = rep(1:3, each = 2),
                 date = rep(as.Date(c("2020-05-01", "2020-06-01")), times = 3),
                 actual_return = c(0.05, 0.02, -0.03, 0.04, 0.01, -0.02),
                 weight = rep(1/3, 6),
                 weight_model = rep("equal_weight", 6),
                 config = rep("config1", 6)
               ))

  # Clean up
  rm("create_portfolios", "add_weight_model", "simple_weight_func", envir = .GlobalEnv)
})
