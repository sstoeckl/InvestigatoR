#' OLS Prediction Function
#'
#' @param train_data Data frame containing `stock_id`, `date`, `return_label`, and feature columns.
#' @param test_data Data frame containing `stock_id`, `date`, and feature columns.
#' @param config List. Configuration parameters for the model (not used in OLS).
#' @param fast Logical. If `TRUE`, use `fastLm` from `RcppArmadillo`; otherwise, use base `lm`.
#'
#' @return Tibble with `stock_id`, `date`, and `pred_return` matching the `test_data`.
#'
#' @importFrom dplyr pull select
#' @importFrom tibble tibble
#' @importFrom stats lm predict
#' @importFrom RcppArmadillo fastLm
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data(data_ml)
#' train_data_ex <- data_ml[1:100, c("stock_id", "date", "R1M_Usd", "Div_Yld", "Eps", "Mkt_Cap_12M_Usd", "Mom_11M_Usd", "Ocf", "Pb", "Vol1Y_Usd")]
#' test_data_ex <- data_ml[101:150, c("stock_id", "date", "Div_Yld", "Eps", "Mkt_Cap_12M_Usd", "Mom_11M_Usd", "Ocf", "Pb", "Vol1Y_Usd")]
#' ols_pred(train_data_ex, test_data_ex, config = list(), fast = TRUE)
#' }
ols_pred <- function(train_data, test_data, config = list(), fast = TRUE) {

  # Input Validation
  required_train_cols <- c("stock_id", "date", "return_label")
  required_test_cols <- c("stock_id", "date")

  if (!all(required_train_cols %in% colnames(train_data))) {
    stop("train_data must contain 'stock_id', 'date', and 'return_label' columns.")
  }

  if (!all(required_test_cols %in% colnames(test_data))) {
    stop("test_data must contain 'stock_id' and 'date' columns.")
  }

  # Handle missing values
  check_missing_values(train_data)
  check_missing_values(test_data)

  # Encode categorical data
  full_data <- bind_rows(train_data, test_data)
  encoded_data <- encode_categorical(full_data)

  # Split data back
  train_encoded <- encoded_data$data[1:nrow(train_data), ]
  test_encoded <- encoded_data$data[(nrow(train_data) + 1):nrow(encoded_data$data), -3]  # Assuming third column is 'return_label'

  # Perform OLS prediction
  if (fast) {
    # Fast version with RcppArmadillo
    mm <- cbind(1, as.matrix(train_encoded[, 4:ncol(train_encoded)]))
    y  <- train_encoded %>% pull(return_label)
    plm <- RcppArmadillo::fastLm(mm, y)

    test_mm <- cbind(1, as.matrix(test_encoded[, 3:ncol(test_encoded)]))
    predictions <- predict(plm, test_mm)
  } else {
    # Standard version with lm
    formula_lm <- as.formula(paste0("return_label ~ ", paste(colnames(train_encoded)[4:ncol(train_encoded)], collapse = " + ")))
    flm <- lm(formula_lm, data = train_encoded)
    predictions <- predict(flm, test_encoded[, 3:ncol(test_encoded)])
  }

  # Return predictions
  predictions <- tibble::tibble(stock_id = test_encoded$stock_id, date = test_encoded$date, pred_return = predictions)
  return(predictions)
}

#' Elastic Net Prediction Function
#'
#' This function trains an Elastic Net model using the glmnet package and predicts outcomes
#' for the provided test data set based on the trained model.
#'
#' @param train_data data frame with stock_id, date, return_label, and features
#' @param test_data data frame with stock_id, date, and features
#' @param config list with Elastic Net parameters (alpha and lambda)
#'
#' @return tibble with stock_id, date, and pred_return matching the test_data
#'
#' @importFrom glmnet glmnet
#' @importFrom tibble tibble
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data(data_ml)
#' train_data_ex <- data_ml[1:100, c(1,2,96,5:10)]
#' test_data_ex <- data_ml[101:150, c(1,2,5:10)]
#' enet_pred(train_data_ex, test_data_ex)
#' }
enet_pred <- function(train_data, test_data, config=list()) {
  # Default Elastic Net parameters
  default_params <- list(
    alpha = 0.5,
    lambda = 0.1
  )

  # Check config
  config <- ensure_config(config, default_params)

  # Handle missing values
  check_missing_values(train_data)
  check_missing_values(test_data)

  # Encode categorical data
  full_data <- bind_rows(train_data, test_data)
  full_data <- encode_categorical(full_data)

  # Split data back
  train_data <- full_data$data[1:nrow(train_data), ]
  test_data <- full_data$data[(nrow(train_data) + 1):nrow(full_data$data), -3]

  # Prepare the model matrix and response vector
  train_features <- as.matrix(train_data[,4:ncol(train_data)])
  train_label <- as.matrix(train_data[,3])

  # Train the Elastic Net model using glmnet
  enet_fit <- glmnet(train_features, train_label, alpha = config$alpha, lambda = config$lambda)

  # Prepare test data and predict
  test_features <- as.matrix(test_data[,3:ncol(test_data)])
  predictions <- predict(enet_fit, s = config$lambda, newx = test_features)

  # Match predictions back to stock_id and date
  predictions <- tibble::tibble(stock_id = test_data$stock_id, date = test_data$date, pred_return = predictions)
  return(predictions)
}

#' XGB function for return prediction
#'
#' @param train_data data frame with stock_id, date, return_label and features
#' @param test_data data frame with stock_id, date and features
#' @param config empty list, as ols does not need any configuration
#' @param fast logical, if TRUE, use fastLm from RcppArmadillo, else use lm from base R
#'
#' @return tibble with stock_id, date and pred_return matching the test_data
#'
#' @import xgboost
#' @importFrom tibble tibble
#' @importFrom stats predict
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data(data_ml)
#' train_data_ex <- data_ml[1:100,c(1,2,96,5:10)]
#' test_data_ex <- data_ml[101:150,c(1,2,5:10)]
#' xgb_pred(train_data_ex, test_data_ex, config=list())
#' }
xgb_pred <- function(train_data, test_data, config = list()) {
  # Default parameters for xgboost
  default_params <- list(
    eta = 0.3,
    max_depth = 4,
    objective = "reg:squarederror",
    nrounds = 80
  )

  # Check config
  config <- ensure_config(config, default_params)

  # Handle missing values
  check_missing_values(train_data)
  check_missing_values(test_data)

  # Encode categorical data
  full_data <- bind_rows(train_data, test_data)
  full_data <- encode_categorical(full_data)

  # Split data back
  train_data <- full_data$data[1:nrow(train_data), ]
  test_data <- full_data$data[(nrow(train_data) + 1):nrow(full_data$data), -3]

  # Prepare data for xgboost
  train_features <- as.matrix(train_data[, 4:ncol(train_data)])
  train_label <- as.matrix(train_data[, 3])
  train_matrix <- xgboost::xgb.DMatrix(data = train_features, label = train_label)

  # Add training data to the config
  config$data <- train_matrix

  # Train the model
  fit <- do.call(xgboost::xgb.train, config)

  # Prepare test data and predict
  test_features <- as.matrix(test_data[, 3:ncol(test_data)])
  test_matrix <- xgboost::xgb.DMatrix(data = test_features)
  predictions <- as.vector(predict(fit, test_matrix))

  # Match predictions back to stock_id and date
  predictions <- tibble::tibble(stock_id = test_data$stock_id, date = test_data$date, pred_return = predictions)

  return(predictions)
}

#' Random Forest Prediction Function
#'
#' @param train_data Data frame containing `stock_id`, `date`, `return_label`, and feature columns.
#' @param test_data Data frame containing `stock_id`, `date`, and feature columns.
#' @param config List. Configuration parameters for the random forest model.
#'  - `num.trees`: Number of trees in the forest (default: 100).
#'  - `mtry`: Number of variables randomly sampled as candidates at each split (default: 5).
#'
#' @return Tibble with `stock_id`, `date`, and `pred_return` matching the `test_data`.
#'
#' @importFrom dplyr pull select
#' @importFrom tibble tibble
#' @importFrom ranger ranger
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data(data_ml)
#' train_data_ex <- data_ml[1:100, c("stock_id", "date", "R1M_Usd", "Div_Yld", "Eps", "Mkt_Cap_12M_Usd", "Mom_11M_Usd", "Ocf", "Pb", "Vol1Y_Usd")]
#' test_data_ex <- data_ml[101:150, c("stock_id", "date", "Div_Yld", "Eps", "Mkt_Cap_12M_Usd", "Mom_11M_Usd", "Ocf", "Pb", "Vol1Y_Usd")]
#' rf_pred(train_data_ex, test_data_ex, config = list(num.trees = 200, mtry = 4))
#' }
rf_pred <- function(train_data, test_data, config = list()) {

  # Define default parameters
  default_params <- list(
    num.trees = 100,  # Number of random trees
    mtry = 5          # Number of variables randomly sampled as candidates at each split
  )

  # Merge user config with default params
  config <- purrr::list_modify(default_params, !!!config)

  # Input Validation
  required_train_cols <- c("stock_id", "date", "return_label")
  required_test_cols <- c("stock_id", "date")

  if (!all(required_train_cols %in% colnames(train_data))) {
    stop("train_data must contain 'stock_id', 'date', and 'return_label' columns.")
  }

  if (!all(required_test_cols %in% colnames(test_data))) {
    stop("test_data must contain 'stock_id' and 'date' columns.")
  }

  # Handle missing values
  check_missing_values(train_data)
  check_missing_values(test_data)

  # Encode categorical data
  full_data <- bind_rows(train_data, test_data)
  encoded_data <- encode_categorical(full_data)

  # Split data back
  train_encoded <- encoded_data$data[1:nrow(train_data), ]
  test_encoded <- encoded_data$data[(nrow(train_data) + 1):nrow(encoded_data$data), -3]  # Assuming third column is 'return_label'

  # Prepare training features and labels
  train_features <- as.matrix(train_encoded[, 4:ncol(train_encoded)])
  train_label <- train_encoded %>% pull(return_label)

  # Train the random forest model using ranger
  fit <- ranger::ranger(
    x = train_features,
    y = train_label,
    num.trees = config$num.trees,
    mtry = config$mtry
  )

  # Prepare test data and predict
  test_features <- as.matrix(test_encoded[, 3:ncol(test_encoded)])
  predictions <- predict(fit, data = test_features)$predictions

  # Return predictions
  predictions <- tibble::tibble(stock_id = test_encoded$stock_id, date = test_encoded$date, pred_return = predictions)
  return(predictions)
}

#' Support Vector Machine (SVM) Prediction Function
#'
#' This function trains a Support Vector Machine (SVM) model to predict returns based on the provided
#' training data and makes predictions for the test data.
#'
#' @param train_data A data frame with stock_id, date, return_label, and features.
#' @param test_data A data frame with stock_id, date, and features.
#' @param config A list of SVM configuration parameters (e.g., kernel, type, gamma, cost, epsilon).
#'
#' @return A tibble with stock_id, date, and predicted returns matching the test data.
#' @importFrom e1071 svm
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows
#' @export
#'
#' @examples
#' \dontrun{
#' data(data_ml)
#' train_data_ex <- data_ml[1:100, c(1,2,96,5:10)]
#' test_data_ex <- data_ml[101:150, c(1,2,5:10)]
#' svm_pred(train_data_ex, test_data_ex, config = list())
#' }
svm_pred <- function(train_data, test_data, config = list()) {
  # Default parameters for SVM
  default_params <- list(
    type = "eps-regression",    # SVM task type (regression in this case)
    kernel = "radial",          # Kernel type (radial, linear, etc.)
    epsilon = 0.1,              # Error tolerance width
    gamma = 0.5,                # Constant in the radial kernel
    cost = 1                    # Penalty for misclassification
  )

  # Ensure config using helper function
  config <- ensure_config(config, default_params)

  # Handle missing values
  check_missing_values(train_data)
  check_missing_values(test_data)

  # Encode categorical data
  full_data <- bind_rows(train_data, test_data)
  full_data <- encode_categorical(full_data)

  # Split data back
  train_data <- full_data$data[1:nrow(train_data), ]
  test_data <- full_data$data[(nrow(train_data) + 1):nrow(full_data$data), -3]

  # Prepare training data
  train_features <- as.matrix(train_data[, 4:ncol(train_data)])
  train_label <- as.matrix(train_data[, 3])

  # Train the SVM model
  model <- e1071::svm(
    x = train_features,
    y = train_label,
    type = config$type,
    kernel = config$kernel,
    epsilon = config$epsilon,
    gamma = config$gamma,
    cost = config$cost
  )

  # Predict using the test data
  test_features <- as.matrix(test_data[, 3:ncol(test_data)])
  predictions <- predict(model, test_features)

  # Match predictions back to stock_id and date
  predictions <- tibble::tibble(stock_id = test_data$stock_id, date = test_data$date, pred_return = predictions)

  return(predictions)
}

#' Neural Network (nnet) Prediction Function
#'
#' This function trains a neural network using the nnet package and makes predictions
#' for the test data set.
#'
#' @param train_data A data frame with stock_id, date, return_label, and features.
#' @param test_data A data frame with stock_id, date, and features.
#' @param config A list containing parameters for the neural network, such as size and decay.
#'
#' @return A tibble with stock_id, date, and predicted returns matching the test data.
#' @importFrom nnet nnet
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows
#' @export
#'
#' @examples
#' \dontrun{
#' data(data_ml)
#' train_data_ex <- data_ml[1:100, ]
#' test_data_ex <- data_ml[101:150, ]
#' nnet_pred(train_data_ex, test_data_ex, config = list())
#' }
nnet_pred <- function(train_data, test_data, config = list()) {
  # Default parameters for neural network
  default_params <- list(
    size = 5,       # Number of units in the hidden layer
    decay = 0.1     # Weight decay (regularization)
  )

  # Ensure config using helper function
  config <- ensure_config(config, default_params)

  # Handle missing values
  check_missing_values(train_data)
  check_missing_values(test_data)

  # Encode categorical data
  full_data <- bind_rows(train_data, test_data)
  full_data <- encode_categorical(full_data)

  # Split data back
  train_data <- full_data$data[1:nrow(train_data), ]
  test_data <- full_data$data[(nrow(train_data) + 1):nrow(full_data$data), -3]

  # Prepare training data
  train_features <- as.matrix(train_data[, 4:ncol(train_data)])
  train_label <- as.matrix(train_data[, 3])

  # Train the neural network model
  model <- nnet::nnet(
    x = train_features,
    y = train_label,
    size = config$size,
    decay = config$decay,
    linout = TRUE
  )

  # Predict using the test data
  test_features <- as.matrix(test_data[, 3:ncol(test_data)])
  predictions <- as.vector(predict(model, test_features))

  # Match predictions back to stock_id and date
  predictions <- tibble::tibble(stock_id = test_data$stock_id, date = test_data$date, pred_return = predictions)

  return(predictions)
}

#' Wrapper for Model Training using Caret
#'
#' Trains machine learning models using caret, supporting various models (e.g., Elastic Net, SVM, etc.)
#' with optional hyperparameter tuning and cross-validation.
#'
#' @param train_data A data frame with stock_id, date, return_label, and features.
#' @param test_data A data frame with stock_id, date, and features.
#' @param config A list of configuration settings, containing the method, tuneGrid, and trControl for caret.
#'
#' @return A tibble with stock_id, date, and predicted returns for the test data.
#' @importFrom caret train
#' @importFrom tibble tibble
#' @export
#'
#' @examples
#' \dontrun{
#' data(data_ml)
#' train_data_ex <- data_ml[1:100, c(1,2,96,5:10)]
#' test_data_ex <- data_ml[101:150, c(1,2,5:10)]
#' config <- list(method = "glmnet", tuneGrid = expand.grid(alpha = 0.5, lambda = 0.1),
#'                trControl = caret::trainControl(method = "cv", number = 5))
#' caret_wrapper(train_data_ex, test_data_ex, config)
#' }
caret_wrapper <- function(train_data, test_data, config = list()) {

  # Ensure the method, tuneGrid, and trControl are provided
  if (!"method" %in% names(config)) stop("Please provide a 'method' for caret model training in the config list.")
  if (!"trControl" %in% names(config)) config$trControl <- caret::trainControl(method = "none")

  # Handle missing values
  check_missing_values(train_data)
  check_missing_values(test_data)

  # Encode categorical data
  full_data <- bind_rows(train_data, test_data)
  full_data <- encode_categorical(full_data)

  # Split data back
  train_data <- full_data$data[1:nrow(train_data), ]
  test_data <- full_data$data[(nrow(train_data) + 1):nrow(full_data$data), -3]

  # Prepare training features and labels
  train_features <- as.matrix(train_data[, 4:ncol(train_data)])
  train_label <- as.matrix(train_data[, 3])

  # Train the model using caret
  model <- caret::train(
    x = train_features,
    y = as.numeric(train_label),
    method = config$method,
    tuneGrid = config$tuneGrid,
    trControl = config$trControl
  )

  # Prepare test data and predict
  test_features <- as.matrix(test_data[, 3:ncol(test_data)])
  predictions <- predict(model, newdata = test_features)

  # Match predictions back to stock_id and date
  predictions <- tibble::tibble(stock_id = test_data$stock_id, date = test_data$date, pred_return = predictions)

  return(predictions)
}
