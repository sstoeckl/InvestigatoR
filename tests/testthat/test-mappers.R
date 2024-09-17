# tests/testthat/test-mappers.R

test_that("retpred_map returns correct predictions", {
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

  # Define indices
  indices <- tibble::tibble(
    training_start = as.Date(c("2020-01-01")),
    training_end = as.Date(c("2020-04-01")),
    prediction_start = as.Date(c("2020-05-01")),
    prediction_end = as.Date(c("2020-05-01"))
  )

  # Define a simple prediction function
  simple_pred_func <- function(train_data, test_data, config) {
    tibble::tibble(
      stock_id = test_data$stock_id,
      date = test_data$date,
      pred_return = mean(train_data$R1M_Usd)
    )
  }

  # Ensure the prediction function is available
  assign("simple_pred_func", simple_pred_func, envir = .GlobalEnv)

  # Call retpred_map
  predictions <- retpred_map(
    t = 1,
    data_subset = data_ml_subset,
    indices = indices,
    model_function = "simple_pred_func",
    model_config = list()
  )

  # Expected predictions
  expected_pred <- mean(data_ml_subset$R1M_Usd[data_ml_subset$date <= as.Date("2020-04-01")])

  # Check
  expect_equal(predictions$pred_return, rep(expected_pred, 3))

  # Clean up
  rm("simple_pred_func", envir = .GlobalEnv)
})
