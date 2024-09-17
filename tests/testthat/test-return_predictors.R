# tests/testthat/test-return_predictors.R

test_that("ols_pred produces reproducible predictions with seed", {
  # Load sample data
  train_data <- tibble::tibble(
    stock_id = rep(1:2, each = 2),
    date = rep(as.Date(c("2020-01-01", "2020-02-01")), times = 2),
    return_label = c(0.05, 0.02, -0.03, 0.04),
    Div_Yld = runif(4, 1, 5),
    Eps = runif(4, 0.5, 2),
    Mkt_Cap_12M_Usd = runif(4, 100, 500),
    Mom_11M_Usd = runif(4, -0.1, 0.1),
    Ocf = runif(4, 10, 50),
    Pb = runif(4, 0.5, 3),
    Vol1Y_Usd = runif(4, 1000, 5000)
  )

  test_data <- tibble::tibble(
    stock_id = rep(3:4, each = 2),
    date = rep(as.Date(c("2020-03-01", "2020-04-01")), times = 2),
    Div_Yld = runif(4, 1, 5),
    Eps = runif(4, 0.5, 2),
    Mkt_Cap_12M_Usd = runif(4, 100, 500),
    Mom_11M_Usd = runif(4, -0.1, 0.1),
    Ocf = runif(4, 10, 50),
    Pb = runif(4, 0.5, 3),
    Vol1Y_Usd = runif(4, 1000, 5000)
  )

  # Define encode_categorical helper function (mock)
  encode_categorical <- function(full_data) {
    list(data = full_data)  # No encoding for simplicity
  }

  # Assign to global environment
  assign("encode_categorical", encode_categorical, envir = .GlobalEnv)

  # Call ols_pred with seed
  predictions1 <- ols_pred(
    train_data = train_data,
    test_data = test_data,
    config = list(),
    fast = TRUE
  )

  predictions2 <- ols_pred(
    train_data = train_data,
    test_data = test_data,
    config = list(),
    fast = TRUE
  )

  # Check that predictions are identical
  expect_equal(predictions1$pred_return, predictions2$pred_return)

  # Clean up
  rm("encode_categorical", envir = .GlobalEnv)
})

test_that("caret_wrapper produces reproducible predictions with seed", {
  # Load sample data
  train_data <- tibble::tibble(
    stock_id = rep(1:2, each = 2),
    date = rep(as.Date(c("2020-01-01", "2020-02-01")), times = 2),
    return_label = c(0.05, 0.02, -0.03, 0.04),
    Div_Yld = runif(4, 1, 5),
    Eps = runif(4, 0.5, 2),
    Mkt_Cap_12M_Usd = runif(4, 100, 500),
    Mom_11M_Usd = runif(4, -0.1, 0.1),
    Ocf = runif(4, 10, 50),
    Pb = runif(4, 0.5, 3),
    Vol1Y_Usd = runif(4, 1000, 5000)
  )

  test_data <- tibble::tibble(
    stock_id = rep(3:4, each = 2),
    date = rep(as.Date(c("2020-03-01", "2020-04-01")), times = 2),
    Div_Yld = runif(4, 1, 5),
    Eps = runif(4, 0.5, 2),
    Mkt_Cap_12M_Usd = runif(4, 100, 500),
    Mom_11M_Usd = runif(4, -0.1, 0.1),
    Ocf = runif(4, 10, 50),
    Pb = runif(4, 0.5, 3),
    Vol1Y_Usd = runif(4, 1000, 5000)
  )

  # Define encode_categorical helper function (mock)
  encode_categorical <- function(full_data) {
    list(data = full_data)  # No encoding for simplicity
  }

  # Assign to global environment
  assign("encode_categorical", encode_categorical, envir = .GlobalEnv)

  # Define a simple caret_wrapper function
  simple_caret_wrapper <- function(train_data, test_data, config = list(), seed = NULL) {
    if (!is.null(seed)) {
      set.seed(seed)
    }

    # For testing, return a constant prediction
    tibble::tibble(
      stock_id = test_data$stock_id,
      date = test_data$date,
      pred_return = 0.1
    )
  }

  # Assign to global environment
  assign("simple_caret_wrapper", simple_caret_wrapper, envir = .GlobalEnv)

  # Define ml_config
  ml_config <- list(
    caret_model = list(
      pred_func = "simple_caret_wrapper",
      config1 = list()
    )
  )

  # Call backtesting_returns with seed
  rp1 <- backtesting_returns(
    data = train_data,
    return_prediction_object = NULL,
    return_label = "R1M_Usd",
    features = c("Div_Yld", "Eps", "Mkt_Cap_12M_Usd", "Mom_11M_Usd", "Ocf", "Pb", "Vol1Y_Usd"),
    rolling = FALSE,
    window_size = "2 months",
    step_size = "1 month",
    offset = "0 months",
    in_sample = FALSE,
    ml_config = ml_config,
    append = FALSE,
    num_cores = 1,
    verbose = FALSE,
    seed = 123
  )

  # Call backtesting_returns again with the same seed
  rp2 <- backtesting_returns(
    data = train_data,
    return_prediction_object = NULL,
    return_label = "R1M_Usd",
    features = c("Div_Yld", "Eps", "Mkt_Cap_12M_Usd", "Mom_11M_Usd", "Ocf", "Pb", "Vol1Y_Usd"),
    rolling = FALSE,
    window_size = "2 months",
    step_size = "1 month",
    offset = "0 months",
    in_sample = FALSE,
    ml_config = ml_config,
    append = FALSE,
    num_cores = 1,
    verbose = FALSE,
    seed = 123
  )

  # Check that predictions are identical
  expect_equal(rp1$predictions, rp2$predictions)

  # Clean up
  rm("encode_categorical", "simple_caret_wrapper", envir = .GlobalEnv)
})

test_that("caret_wrapper with glmnet produces reproducible predictions", {
  # Load sample data
  train_data <- tibble::tibble(
    stock_id = rep(1:2, each = 2),
    date = rep(as.Date(c("2020-01-01", "2020-02-01")), times = 2),
    return_label = c(0.05, 0.02, -0.03, 0.04),
    Div_Yld = c(2.0, 3.0, 1.5, 4.0),
    Eps = c(1.0, 1.5, 0.8, 2.0),
    Mkt_Cap_12M_Usd = c(200, 300, 150, 400),
    Mom_11M_Usd = c(0.05, -0.02, 0.03, 0.01),
    Ocf = c(20, 30, 15, 40),
    Pb = c(1.2, 2.5, 0.9, 3.0),
    Vol1Y_Usd = c(2000, 3000, 1500, 4000)
  )

  test_data <- tibble::tibble(
    stock_id = rep(3:4, each = 2),
    date = rep(as.Date(c("2020-03-01", "2020-04-01")), times = 2),
    Div_Yld = c(2.5, 3.5, 1.8, 4.2),
    Eps = c(1.2, 1.6, 0.9, 2.2),
    Mkt_Cap_12M_Usd = c(250, 350, 180, 420),
    Mom_11M_Usd = c(0.04, -0.01, 0.02, 0.00),
    Ocf = c(25, 35, 18, 45),
    Pb = c(1.3, 2.7, 1.0, 3.1),
    Vol1Y_Usd = c(2500, 3500, 1800, 4200)
  )

  # Define encode_categorical helper function (mock)
  encode_categorical <- function(full_data) {
    list(data = full_data)  # No encoding for simplicity
  }

  # Assign to global environment
  assign("encode_categorical", encode_categorical, envir = .GlobalEnv)

  # Define a simple ml_config for glmnet
  ml_config <- list(
    glmnet_model = list(
      pred_func = "caret_wrapper",
      config1 = list(
        method = "glmnet",
        tuneGrid = expand.grid(alpha = 0.5, lambda = 0.1),
        trControl = caret::trainControl(method = "none")
      )
    )
  )

  # Call backtesting_returns with seed
  rp1 <- backtesting_returns(
    data = train_data,
    return_prediction_object = NULL,
    return_label = "R1M_Usd",
    features = c("Div_Yld", "Eps", "Mkt_Cap_12M_Usd", "Mom_11M_Usd", "Ocf", "Pb", "Vol1Y_Usd"),
    rolling = FALSE,
    window_size = "2 months",
    step_size = "1 month",
    offset = "0 months",
    in_sample = FALSE,
    ml_config = ml_config,
    append = FALSE,
    num_cores = 1,
    verbose = FALSE,
    seed = 123
  )

  rp2 <- backtesting_returns(
    data = train_data,
    return_prediction_object = NULL,
    return_label = "R1M_Usd",
    features = c("Div_Yld", "Eps", "Mkt_Cap_12M_Usd", "Mom_11M_Usd", "Ocf", "Pb", "Vol1Y_Usd"),
    rolling = FALSE,
    window_size = "2 months",
    step_size = "1 month",
    offset = "0 months",
    in_sample = FALSE,
    ml_config = ml_config,
    append = FALSE,
    num_cores = 1,
    verbose = FALSE,
    seed = 123
  )

  # Check that predictions are identical
  expect_equal(rp1$predictions, rp2$predictions)

  # Clean up
  rm("encode_categorical", envir = .GlobalEnv)
})
