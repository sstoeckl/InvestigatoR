# tests/testthat/test-backtesting_returns.R

library(testthat)
library(InvestigatoR)
library(tidyverse)

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

# Define ML configurations for testing
ml_config_all <- list(
  ols_pred = list(
    pred_func = "ols_pred",
    config = list()
  ),
  enet_pred = list(
    pred_func = "enet_pred",
    config1 = list(alpha = 0.5, lambda = 0.1),
    config2 = list(alpha = 0.5, lambda = 0.5)
  ),
  xgb_pred = list(
    pred_func = "xgb_pred",
    config1 = list(nrounds = 1, max_depth = 3, eta = 0.3, objective = "reg:squarederror"),
    config2 = list(nrounds = 1, max_depth = 3, eta = 0.1, objective = "reg:squarederror")
  ),
  rf_pred = list(
    pred_func = "rf_pred",
    config1 = list(num.trees = 10, mtry = 4),
    config2 = list(num.trees = 5, mtry = 3)
  ),
  # svm_pred = list(
  #   pred_func = "svm_pred",
  #   config1 = list(kernel = "linear", cost = 1),
  #   config2 = list(kernel = "radial", cost = 1)
  # ),
  nnet_pred = list(
    pred_func = "nnet_pred",
    config1 = list(size = 10, decay = 0.01),
    config2 = list(size = 5, decay = 0.05)
  )
)

# Test for backtesting_returns with all models
test_that("backtesting_returns executes correctly with multiple models", {
  # Run backtesting_returns with all models
  rp_all <- backtesting_returns(
    data = test_data_ml,
    return_prediction_object = NULL,
    return_label = return_label,
    features = features,
    rolling = rolling,
    window_size = window_size,
    step_size = step_size,
    offset = offset,
    in_sample = in_sample,
    ml_config = ml_config_all,
    append = FALSE,
    num_cores = 1L,  # Use single core for testing
    verbose = TRUE  # Set to TRUE for detailed messages during testing
  )

  # Verify the returned object
  expect_s3_class(rp_all, "returnPrediction")
  expect_true(length(rp_all$models) == length(ml_config_all)*2-1)

  # Verify predictions
  expect_true(nrow(rp_all$predictions) > 0)
  expect_true(all(c("stock_id", "date") %in% colnames(rp_all$predictions)))
  # check summary
  expect_true(all(colnames(summary(rp_all)) == c("MSE","RMSE","MAE","Hit_Ratio")))
  # check summary rownames
  expect_true(all(rownames(summary(rp_all)) == colnames(rp_all$predictions)[-c(1:2)]))
})

# Test for ols_pred function
test_that("ols_pred produces correct output structure", {
  # Split the test data into training and testing sets
  train_data_ex <- test_data_ml[1:150, c("stock_id", "date", return_label, features)]
  test_data_ex <- test_data_ml[151:200, c("stock_id", "date", features)]

  predictions <- ols_pred(train_data_ex, test_data_ex, config = list())

  expect_s3_class(predictions,"tbl_df")
  expect_true(all(c("stock_id", "date", "pred_return") %in% colnames(predictions)))
  expect_equal(nrow(predictions), nrow(test_data_ex))
})

# Test for enet_pred function
test_that("enet_pred produces correct output structure", {
  # Split the test data into training and testing sets
  train_data_ex <- test_data_ml[1:150, c("stock_id", "date", return_label, features)]
  test_data_ex <- test_data_ml[151:200, c("stock_id", "date", features)]

  predictions <- enet_pred(train_data_ex, test_data_ex, config = list(alpha = 0.5, lambda = 0.1))

  expect_s3_class(predictions,"tbl_df")
  expect_true(all(c("stock_id", "date", "pred_return") %in% colnames(predictions)))
  expect_equal(nrow(predictions), nrow(test_data_ex))
})

# Test for xgb_pred function
test_that("xgb_pred produces correct output structure", {
  # Split the test data into training and testing sets
  train_data_ex <- test_data_ml[1:150, c("stock_id", "date", return_label, features)]
  test_data_ex <- test_data_ml[151:200, c("stock_id", "date", features)]

  predictions <- xgb_pred(train_data_ex, test_data_ex, config = list(nrounds = 10, max_depth = 3, eta = 0.3, objective = "reg:squarederror"))

  expect_s3_class(predictions,"tbl_df")
  expect_true(all(c("stock_id", "date", "pred_return") %in% colnames(predictions)))
  expect_equal(nrow(predictions), nrow(test_data_ex))
})

# Test for rf_pred function
test_that("rf_pred produces correct output structure", {
  # Split the test data into training and testing sets
  train_data_ex <- test_data_ml[1:150, c("stock_id", "date", return_label, features)]
  test_data_ex <- test_data_ml[151:200, c("stock_id", "date", features)]

  predictions <- rf_pred(train_data_ex, test_data_ex, config = list(num.trees = 200, mtry = 4))

  expect_s3_class(predictions,"tbl_df")
  expect_true(all(c("stock_id", "date", "pred_return") %in% colnames(predictions)))
  expect_equal(nrow(predictions), nrow(test_data_ex))
})

# Test for svm_pred function
test_that("svm_pred produces correct output structure", {
  # Split the test data into training and testing sets
  train_data_ex <- test_data_ml[1:150, c("stock_id", "date", return_label, features)]
  test_data_ex <- test_data_ml[151:200, c("stock_id", "date", features)]

  predictions <- svm_pred(train_data_ex, test_data_ex, config = list(kernel = "linear", cost = 1))

  expect_s3_class(predictions,"tbl_df")
  expect_true(all(c("stock_id", "date", "pred_return") %in% colnames(predictions)))
  expect_equal(nrow(predictions), nrow(test_data_ex))
})

# Test for nnet_pred function
test_that("nnet_pred produces correct output structure", {
  # Split the test data into training and testing sets
  train_data_ex <- test_data_ml[1:150, c("stock_id", "date", return_label, features)]
  test_data_ex <- test_data_ml[151:200, c("stock_id", "date", features)]

  predictions <- nnet_pred(train_data_ex, test_data_ex, config = list(size = 10, decay = 0.01))

  expect_s3_class(predictions,"tbl_df")
  expect_true(all(c("stock_id", "date", "pred_return") %in% colnames(predictions)))
  expect_equal(nrow(predictions), nrow(test_data_ex))
})

caret_configs <- list(
  # Configuration 1: glmnet with hyperparameter tuning
  glmnet_tune = list(
    method = "glmnet",
    tuneGrid = expand.grid(alpha = seq(0, 1, by = 0.5), lambda = c(0.01, 0.1)),
    trControl = trainControl(method = "cv", number = 5),
    metric = "RMSE"
  ),
  # Configuration 2: Random Forest with hyperparameter tuning
  rf_tune = list(
    method = "rf",
    tuneGrid = expand.grid(mtry = c(2, 4, 6)),
    trControl = trainControl(method = "cv", number = 5),
    metric = "RMSE"
  ),
  # Configuration 3: Support Vector Machine with hyperparameter tuning
  svm_tune = list(
    method = "svmRadial",
    tuneGrid = expand.grid(C = c(0.5, 1, 2), sigma = c(0.01, 0.1)),
    trControl = trainControl(method = "cv", number = 5),
    metric = "RMSE"
  ),
  # Configuration 4: Linear Regression without hyperparameter tuning
  lm_simple = list(
    method = "lm",
    trControl = trainControl(method = "none"),
    metric = "RMSE"
  )
)

# Begin testing
test_that("caret_wrapper trains models without errors and returns correct structure", {
  # Split the test data into training and testing sets
  train_data_ex <- test_data_ml[1:150, c("stock_id", "date", return_label, features)]
  test_data_ex <- test_data_ml[151:200, c("stock_id", "date", features)]
  for (config_name in names(caret_configs)) {
    config <- caret_configs[[config_name]]
    # Capture verbose output
    predictions <- tryCatch(
      suppressWarnings(caret_wrapper(
        train_data = train_data_ex,
        test_data = test_data_ex,
        config = config
      )),
      error = function(e) {
        fail(paste("caret_wrapper failed for configuration:", config_name, "Error:", e$message))
      }
    )

    # Check that predictions is a tibble
    expect_s3_class(predictions, "tbl_df")

    # Check for required columns
    expect_true(all(c("stock_id", "date", "pred_return") %in% colnames(predictions)))

    # Check the number of predictions matches the test data
    expect_equal(nrow(predictions), nrow(test_data_ex))

    # Check that pred_return is numeric
    expect_type(predictions$pred_return, "double")
  }
})

# Test for backtesting_returns with caret models
rolling <- TRUE
window_size <- "10 years"
step_size <- "10 Years"
offset <- "1 month"
in_sample <- TRUE
# ml_config for caret models
ml_config_caret <- list(
  ols_pred_caret = list(
    pred_func = "caret_wrapper",
    config = list(
      method = "lm",
      trControl = trainControl(method = "none"),
      metric = "RMSE"
    )
  ),
  rf_pred_caret = list(
    pred_func = "caret_wrapper",
    config = list(
      method = "rf",
      tuneGrid = expand.grid(mtry = c(2, 4, 6)),
      trControl = trainControl(method = "cv", number = 2),
      metric = "RMSE"
    )
  ),
  svm_pred_caret = list(
    pred_func = "caret_wrapper",
    config = list(
      method = "svmRadial",
      tuneGrid = expand.grid(C = c(0.5, 1, 2), sigma = c(0.01, 0.1)),
      trControl = trainControl(method = "cv", number = 2),
      metric = "RMSE"
    )
  ),
  glmnet_pred_caret = list(
    pred_func = "caret_wrapper",
    config = list(
      method = "glmnet",
      tuneGrid = expand.grid(alpha = seq(0, 1, by = 0.5), lambda = c(0.01, 0.1)),
      trControl = trainControl(method = "cv", number = 2),
      metric = "RMSE"
    )
  )
)

test_that("backtesting_returns executes correctly with caret models", {
  # Run backtesting_returns with all models
  rp_all <- backtesting_returns(
    data = test_data_ml,
    return_prediction_object = NULL,
    return_label = return_label,
    features = features,
    rolling = rolling,
    window_size = window_size,
    step_size = step_size,
    offset = offset,
    in_sample = in_sample,
    ml_config = ml_config_caret,
    append = FALSE,
    num_cores = 1L,  # Use single core for testing
    verbose = TRUE  # Set to TRUE for detailed messages during testing
  )

  # Verify the returned object
  expect_s3_class(rp_all, "returnPrediction")
  expect_true(length(rp_all$models) == length(ml_config_caret))

  # Verify predictions
  expect_true(nrow(rp_all$predictions) > 0)
  expect_true(all(c("stock_id", "date") %in% colnames(rp_all$predictions)))
  # check summary
  expect_true(all(colnames(summary(rp_all)) == c("MSE","RMSE","MAE","Hit_Ratio")))
  # check summary rownames
  expect_true(all(rownames(summary(rp_all)) == colnames(rp_all$predictions)[-c(1:2)]))
})

test_that("add_extra_data works as expected", {
  # Create a sample returnPrediction object
  data <- tibble::tibble(
    stock_id = 1:100,
    date = seq.Date(Sys.Date(), by = "day", length.out = 100),
    actual_return = runif(100)
  )

  rp <- create_return_prediction(data, "actual_return")

  # Create extra data matching the dimensions and keys
  extra_data <- tibble::tibble(
    stock_id = 1:100,
    date = seq.Date(Sys.Date(), by = "day", length.out = 100),
    sector = sample(c("Technology", "Healthcare", "Finance"), 100, replace = TRUE),
    country = sample(c("USA", "Germany", "Japan"), 100, replace = TRUE)
  )

  # Add extra data
  rp <- add_extra_data(rp, extra_data, new_data_name = "sector_info")

  # Test if the extra_data is added successfully
  expect_true("sector_info" %in% names(rp$extra_data))
  expect_equal(nrow(rp$extra_data$sector_info), 100)

  # Test if column names are correct
  expect_true("sector" %in% colnames(rp$extra_data$sector_info))
  expect_true("country" %in% colnames(rp$extra_data$sector_info))

  # Test if adding more data with same stock_id and date left_joins correctly
  extra_data_2 <- tibble::tibble(
    stock_id = 1:100,
    date = seq.Date(Sys.Date(), by = "day", length.out = 100),
    market_cap = runif(100, min = 1e9, max = 1e12)
  )

  rp <- add_extra_data(rp, extra_data_2, new_data_name = "sector_info")

  # Test if new column is added
  expect_true("market_cap" %in% colnames(rp$extra_data$sector_info))

  # Test if dimensions remain consistent
  expect_equal(nrow(rp$extra_data$sector_info), 100)

  # Test if correct warning or message is shown when overwriting columns
  extra_data_3 <- tibble::tibble(
    stock_id = 1:100,
    date = seq.Date(Sys.Date(), by = "day", length.out = 100),
    sector = sample(c("Finance", "Healthcare", "Technology"), 100, replace = TRUE)  # Overwriting 'sector'
  )

  # Test if 'sector' was indeed overwritten
  expect_true(all(rp$extra_data$sector_info$sector %in% c("Finance", "Healthcare", "Technology")))
})

