# tests/testthat/test-weight_functions.R

test_that("quantile_weights returns correct weight structure", {
  # Load sample data
  return_predictions <- tibble::tibble(
    stock_id = rep(1:3, each = 2),
    date = rep(as.Date(c("2020-05-01", "2020-06-01")), times = 3),
    pred_col1 = c(0.1, 0.2, 0.3, -0.1, 0.5, -0.2),
    pred_col2 = c(-0.3, 0.4, 0.2, 0.1, -0.2, 0.3)
  )

  errors <- tibble::tibble(
    stock_id = rep(1:3, each = 2),
    date = rep(as.Date(c("2020-05-01", "2020-06-01")), times = 3),
    error_col1 = runif(6, 0.1, 0.5),
    error_col2 = runif(6, 0.1, 0.5)
  )

  # Define constraints
  constraints <- list(
    quantiles = list(long = 0.25, short = 0.25),
    allow_short_sale = TRUE,
    min_weight = -0.1,
    max_weight = 0.1,
    b = 1
  )

  # Call quantile_weights
  weights <- quantile_weights(return_predictions, errors, constraints)

  # Check structure
  expect_true("stock_id" %in% colnames(weights))
  expect_true("date" %in% colnames(weights))
  expect_true(all(c("pred_col1", "pred_col2") %in% colnames(weights)))

  # Check that weights are within constraints
  expect_true(all(weights$pred_col1 >= -0.1 & weights$pred_col1 <= 0.1))
  expect_true(all(weights$pred_col2 >= -0.1 & weights$pred_col2 <= 0.1))

  # Check that sum of weights per date and short is approximately 1 or -1 based on short sales
  # (This depends on the implementation; adjust accordingly)
})

test_that("ensemble_weights returns correct predictions", {
  # Load sample data
  return_predictions <- tibble::tibble(
    stock_id = rep(1:3, each = 2),
    date = rep(as.Date(c("2020-05-01", "2020-06-01")), times = 3),
    pred1 = c(0.1, 0.2, 0.3, -0.1, 0.5, -0.2),
    pred2 = c(-0.3, 0.4, 0.2, 0.1, -0.2, 0.3)
  )

  errors <- tibble::tibble(
    stock_id = rep(1:3, each = 2),
    date = rep(as.Date(c("2020-05-01", "2020-06-01")), times = 3),
    error1 = runif(6, 0.1, 0.5),
    error2 = runif(6, 0.1, 0.5)
  )

  # Simple Average
  ensemble_simple <- ensemble_weights(return_predictions, errors, method = "simple_average")
  expected_simple <- rowMeans(return_predictions %>% select(pred1, pred2))
  expect_equal(ensemble_simple, expected_simple)

  # Weighted Average
  # Calculate expected weights
  average_errors <- rowMeans(errors %>% select(error1, error2))
  weights <- 1 / average_errors
  weights <- weights / sum(weights)
  expected_weighted <- as.numeric(as.matrix(return_predictions %>% select(pred1, pred2)) %*% weights)
  ensemble_weighted <- ensemble_weights(return_predictions, errors, method = "weighted_average")
  expect_equal(ensemble_weighted, expected_weighted)

  # Error Covariance
  cov_E <- cov(as.matrix(errors %>% select(error1, error2)), use = "pairwise.complete.obs")
  inv_cov_E <- solve(cov_E)
  one_vector <- rep(1, ncol(cov_E))
  weights_ec <- inv_cov_E %*% one_vector
  weights_ec <- weights_ec / sum(weights_ec)
  expected_ec <- as.numeric(as.matrix(return_predictions %>% select(pred1, pred2)) %*% weights_ec)
  ensemble_ec <- ensemble_weights(return_predictions, errors, method = "error_covariance")
  expect_equal(ensemble_ec, expected_ec)
})
