#' Ordinary Least Squares (OLS) Prediction Function
#'
#' This function trains an Ordinary Least Squares (OLS) regression model to predict returns based on the provided training data
#' and predicts returns for the test data.
#'
#' @param train_data A data frame with `stock_id`, `date`, `return_label`, and features.
#' @param test_data A data frame with `stock_id`, `date`, and features.
#' @param config A list of OLS configuration parameters (e.g., `intercept`).
#'
#' @return A tibble with `stock_id`, `date`, and `pred_return` matching the test data.
#' @importFrom stats lm predict
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows
#' @importFrom checkmate assert_data_frame assert_list
#' @importFrom cli cli_abort cli_alert_danger
#' @export
#'
#' @examples
#' \dontrun{
#' data(data_ml)
#' train_data_ex <- data_ml[1:100, c("stock_id", "date", "R1M_Usd", "Div_Yld", "Eps", "Mkt_Cap_12M_Usd", "Mom_11M_Usd", "Ocf", "Pb", "Vol1Y_Usd")]
#' test_data_ex <- data_ml[101:150, c("stock_id", "date", "Div_Yld", "Eps", "Mkt_Cap_12M_Usd", "Mom_11M_Usd", "Ocf", "Pb", "Vol1Y_Usd")]
#' ols_pred(train_data_ex, test_data_ex, config = list(intercept = TRUE))
#' }
ols_pred <- function(train_data, test_data, config = list()) {
  # Input Validation
  checkmate::assert_data_frame(train_data, min.rows = 1, any.missing = FALSE, .var.name = "train_data")
  checkmate::assert_data_frame(test_data, min.rows = 1, any.missing = FALSE, .var.name = "test_data")
  checkmate::assert_list(config, .var.name = "config")

  # Default parameters for OLS
  default_params <- list(
    intercept = TRUE  # Whether to include an intercept in the model
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
  return_label <- colnames(train_data)[3]
  test_data <- full_data$data[(nrow(train_data) + 1):nrow(full_data$data), -which(names(full_data$data) == return_label)]

  # Prepare formula for lm
  features <- names(train_data)[4:ncol(train_data)]
  formula <- as.formula(paste("`", return_label, "` ~ ", paste(features, collapse = " + "), sep = ""))

  # Train the OLS model
  model <- tryCatch(
    stats::lm(formula = formula, data = train_data),
    error = function(e) {
      cli::cli_abort("Error training OLS model: {e$message}")
    }
  )

  # Predict using the test data
  predictions_matrix <- tryCatch(
    stats::predict(model, newdata = test_data),
    error = function(e) {
      cli::cli_abort("Error during OLS prediction: {e$message}")
    }
  )

  # Convert predictions to numeric vector
  predictions <- as.vector(predictions_matrix)

  # Match predictions back to stock_id and date
  predictions <- tibble::tibble(
    stock_id = test_data$stock_id,
    date = test_data$date,
    pred_return = predictions
  )

  return(predictions)
}

#' Elastic Net Prediction Function
#'
#' This function trains an Elastic Net model using the glmnet package and predicts outcomes
#' for the provided test data set based on the trained model.
#'
#' @param train_data A data frame with `stock_id`, `date`, `return_label`, and features.
#' @param test_data A data frame with `stock_id`, `date`, and features.
#' @param config A list of Elastic Net parameters (`alpha` and `lambda`).
#'
#' @return A tibble with `stock_id`, `date`, and `pred_return` matching the test data.
#' @importFrom glmnet glmnet
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows
#' @importFrom checkmate assert_data_frame assert_list
#' @importFrom cli cli_abort cli_alert_danger
#' @export
#'
#' @examples
#' \dontrun{
#' data(data_ml)
#' train_data_ex <- data_ml[1:100, c("stock_id", "date", "R1M_Usd", "Div_Yld", "Eps", "Mkt_Cap_12M_Usd", "Mom_11M_Usd", "Ocf", "Pb", "Vol1Y_Usd")]
#' test_data_ex <- data_ml[101:150, c("stock_id", "date", "Div_Yld", "Eps", "Mkt_Cap_12M_Usd", "Mom_11M_Usd", "Ocf", "Pb", "Vol1Y_Usd")]
#' enet_pred(train_data_ex, test_data_ex, config = list(alpha = 0.5, lambda = 0.1))
#' }
enet_pred <- function(train_data, test_data, config = list()) {
  # Input Validation
  checkmate::assert_data_frame(train_data, min.rows = 1, any.missing = FALSE, .var.name = "train_data")
  checkmate::assert_data_frame(test_data, min.rows = 1, any.missing = FALSE, .var.name = "test_data")
  checkmate::assert_list(config, .var.name = "config")

  # Default Elastic Net parameters
  default_params <- list(
    alpha = 0.5,
    lambda = 0.1
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
  return_label <- colnames(train_data)[3]
  test_data <- full_data$data[(nrow(train_data) + 1):nrow(full_data$data), -which(names(full_data$data) == return_label)]

  # Prepare the model matrix and response vector
  train_features <- as.matrix(train_data[, 4:ncol(train_data)])
  train_label <- train_data[[3]]  # Assuming the 3rd column is the response variable

  # Train the Elastic Net model using glmnet
  enet_fit <- tryCatch(
    glmnet::glmnet(train_features, train_label, alpha = config$alpha, lambda = config$lambda),
    error = function(e) {
      cli::cli_abort("Error training Elastic Net model: {e$message}")
    }
  )

  # Prepare test data and predict
  test_features <- as.matrix(test_data[, 3:ncol(test_data)])
  predictions_matrix <- tryCatch(
    predict(enet_fit, s = config$lambda, newx = test_features),
    error = function(e) {
      cli::cli_abort("Error during Elastic Net prediction: {e$message}")
    }
  )

  # Convert predictions to a numeric vector
  predictions <- as.vector(predictions_matrix)

  # Match predictions back to stock_id and date
  predictions <- tibble::tibble(
    stock_id = test_data$stock_id,
    date = test_data$date,
    pred_return = predictions
  )

  return(predictions)
}

#' Extreme Gradient Boosting (XGBoost) Prediction Function
#'
#' This function trains an XGBoost model to predict returns based on the provided training data
#' and predicts returns for the test data.
#'
#' @param train_data A data frame with `stock_id`, `date`, `return_label`, and features.
#' @param test_data A data frame with `stock_id`, `date`, and features.
#' @param config A list of XGBoost configuration parameters (e.g., `nrounds`, `max_depth`, `eta`).
#'
#' @return A tibble with `stock_id`, `date`, and `pred_return` matching the test data.
#' @import xgboost
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows
#' @importFrom checkmate assert_data_frame assert_list
#' @importFrom cli cli_abort cli_alert_danger
#' @export
#'
#' @examples
#' \dontrun{
#' data(data_ml)
#' train_data_ex <- data_ml[1:100, c("stock_id", "date", "R1M_Usd", "Div_Yld", "Eps", "Mkt_Cap_12M_Usd", "Mom_11M_Usd", "Ocf", "Pb", "Vol1Y_Usd")]
#' test_data_ex <- data_ml[101:150, c("stock_id", "date", "Div_Yld", "Eps", "Mkt_Cap_12M_Usd", "Mom_11M_Usd", "Ocf", "Pb", "Vol1Y_Usd")]
#' xgb_pred(train_data_ex, test_data_ex, config = list(nrounds = 100, max_depth = 3, eta = 0.3, objective = "reg:squarederror"))
#' }
xgb_pred <- function(train_data, test_data, config = list()) {
  # Input Validation
  checkmate::assert_data_frame(train_data, min.rows = 1, any.missing = FALSE, .var.name = "train_data")
  checkmate::assert_data_frame(test_data, min.rows = 1, any.missing = FALSE, .var.name = "test_data")
  checkmate::assert_list(config, .var.name = "config")

  # Default parameters for XGBoost
  default_params <- list(
    nrounds = 100,                 # Number of boosting iterations
    max_depth = 3,                 # Maximum tree depth for base learners
    eta = 0.3,                     # Learning rate
    objective = "reg:squarederror" # Learning objective
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
  return_label <- colnames(train_data)[3]
  test_data <- full_data$data[(nrow(train_data) + 1):nrow(full_data$data), -which(names(full_data$data) == return_label)]

  # Prepare training matrix and labels
  train_matrix <- as.matrix(train_data[, 4:ncol(train_data)])
  train_label <- train_data[[3]]  # Assuming the 3rd column is the response variable

  dtrain <- xgboost::xgb.DMatrix(data = train_matrix, label = train_label)

  # Train the XGBoost model
  model <- tryCatch(
    xgboost::xgboost(
      data = dtrain,
      nrounds = config$nrounds,
      max_depth = config$max_depth,
      eta = config$eta,
      objective = config$objective,
      verbose = 0
    ),
    error = function(e) {
      cli::cli_abort("Error training XGBoost model: {e$message}")
    }
  )

  # Prepare test matrix and predict
  test_matrix <- as.matrix(test_data[, 3:ncol(test_data)])
  predictions_matrix <- tryCatch(
    predict(model, test_matrix),
    error = function(e) {
      cli::cli_abort("Error during XGBoost prediction: {e$message}")
    }
  )

  # Convert predictions to numeric vector
  predictions <- as.vector(predictions_matrix)

  # Match predictions back to stock_id and date
  predictions <- tibble::tibble(
    stock_id = test_data$stock_id,
    date = test_data$date,
    pred_return = predictions
  )

  return(predictions)
}

#' Random Forest Function for Return Prediction
#'
#' This function trains a Random Forest model to predict returns based on the provided training data
#' and predicts returns for the test data.
#'
#' @param train_data A data frame with `stock_id`, `date`, `return_label`, and features.
#' @param test_data A data frame with `stock_id`, `date`, and features.
#' @param config A list of Random Forest configuration parameters (e.g., `num.trees`, `mtry`).
#'
#' @return A tibble with `stock_id`, `date`, and `pred_return` matching the test data.
#' @import ranger
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows
#' @importFrom checkmate assert_data_frame assert_list
#' @importFrom cli cli_abort cli_alert_danger
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
  # Input Validation
  checkmate::assert_data_frame(train_data, min.rows = 1, any.missing = FALSE, .var.name = "train_data")
  checkmate::assert_data_frame(test_data, min.rows = 1, any.missing = FALSE, .var.name = "test_data")
  checkmate::assert_list(config, .var.name = "config")

  # Default parameters for ranger
  default_params <- list(
    num.trees = 100,  # Number of random trees
    mtry = 5          # Number of variables randomly sampled as candidates at each split
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
  return_label <- colnames(train_data)[3]
  test_data <- full_data$data[(nrow(train_data) + 1):nrow(full_data$data), -which(names(full_data$data) == return_label)]

  # Prepare training features and labels
  train_features <- as.matrix(train_data[, 4:ncol(train_data)])
  train_label <- train_data[[3]]  # Assuming the 3rd column is the response variable

  # Train the Random Forest model using ranger
  fit <- tryCatch(
    ranger::ranger(
      x = train_features,
      y = train_label,
      num.trees = config$num.trees,
      mtry = config$mtry
    ),
    error = function(e) {
      cli::cli_abort("Error training Random Forest model: {e$message}")
    }
  )

  # Prepare test data and predict
  test_features <- as.matrix(test_data[, 3:ncol(test_data)])
  predictions <- tryCatch(
    predict(fit, data = test_features)$predictions,
    error = function(e) {
      cli::cli_abort("Error during Random Forest prediction: {e$message}")
    }
  )

  # Ensure predictions are numeric vectors
  predictions <- as.vector(predictions)

  # Match predictions back to stock_id and date
  predictions <- tibble::tibble(
    stock_id = test_data$stock_id,
    date = test_data$date,
    pred_return = predictions
  )

  return(predictions)
}

#' Neural Network (nnet) Prediction Function
#'
#' This function trains a neural network using the nnet package and makes predictions
#' for the test data set.
#'
#' @param train_data A data frame with `stock_id`, `date`, `return_label`, and features.
#' @param test_data A data frame with `stock_id`, `date`, and features.
#' @param config A list containing parameters for the neural network, such as `size` and `decay`.
#'
#' @return A tibble with `stock_id`, `date`, and `pred_return` matching the test data.
#' @importFrom nnet nnet
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows
#' @importFrom checkmate assert_data_frame assert_list
#' @importFrom cli cli_abort cli_alert_danger
#' @export
#'
#' @examples
#' \dontrun{
#' data(data_ml)
#' train_data_ex <- data_ml[1:100, c("stock_id", "date", "R1M_Usd", "Div_Yld", "Eps", "Mkt_Cap_12M_Usd", "Mom_11M_Usd", "Ocf", "Pb", "Vol1Y_Usd")]
#' test_data_ex <- data_ml[101:150, c("stock_id", "date", "Div_Yld", "Eps", "Mkt_Cap_12M_Usd", "Mom_11M_Usd", "Ocf", "Pb", "Vol1Y_Usd")]
#' nnet_pred(train_data_ex, test_data_ex, config = list(size = 10, decay = 0.01))
#' }
nnet_pred <- function(train_data, test_data, config = list()) {
  # Input Validation
  checkmate::assert_data_frame(train_data, min.rows = 1, any.missing = FALSE, .var.name = "train_data")
  checkmate::assert_data_frame(test_data, min.rows = 1, any.missing = FALSE, .var.name = "test_data")
  checkmate::assert_list(config, .var.name = "config")

  # Default parameters for neural network
  default_params <- list(
    size = 5,    # Number of units in the hidden layer
    decay = 0.1  # Weight decay (regularization)
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
  return_label <- colnames(train_data)[3]
  test_data <- full_data$data[(nrow(train_data) + 1):nrow(full_data$data), -which(names(full_data$data) == return_label)]

  # Prepare training data
  train_features <- as.matrix(train_data[, 4:ncol(train_data)])
  train_label <- train_data[[3]]  # Assuming the 3rd column is the response variable

  # Train the neural network model using nnet
  model <- tryCatch(
    nnet::nnet(
      x = train_features,
      y = train_label,
      size = config$size,
      decay = config$decay,
      linout = TRUE,
      trace = FALSE
    ),
    error = function(e) {
      cli::cli_abort("Error training Neural Network model: {e$message}")
    }
  )

  # Predict using the test data
  test_features <- as.matrix(test_data[, 3:ncol(test_data)])
  predictions <- tryCatch(
    predict(model, test_features),
    error = function(e) {
      cli::cli_abort("Error during Neural Network prediction: {e$message}")
    }
  )

  # Ensure predictions are numeric vectors
  predictions <- as.vector(predictions)

  # Match predictions back to stock_id and date
  predictions <- tibble::tibble(
    stock_id = test_data$stock_id,
    date = test_data$date,
    pred_return = predictions
  )

  return(predictions)
}

#' Support Vector Machine (SVM) Prediction Function
#'
#' This function trains a Support Vector Machine (SVM) model to predict returns based on the provided
#' training data and makes predictions for the test data.
#'
#' @param train_data A data frame with `stock_id`, `date`, `return_label`, and features.
#' @param test_data A data frame with `stock_id`, `date`, and features.
#' @param config A list of SVM configuration parameters (e.g., `kernel`, `type`, `gamma`, `cost`, `epsilon`).
#'
#' @return A tibble with `stock_id`, `date`, and `pred_return` matching the test data.
#' @importFrom e1071 svm
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows
#' @importFrom checkmate assert_data_frame assert_list
#' @importFrom cli cli_abort cli_alert_danger
#' @export
#'
#' @examples
#' \dontrun{
#' data(data_ml)
#' train_data_ex <- data_ml[1:100, c("stock_id", "date", "R1M_Usd", "Div_Yld", "Eps", "Mkt_Cap_12M_Usd", "Mom_11M_Usd", "Ocf", "Pb", "Vol1Y_Usd")]
#' test_data_ex <- data_ml[101:150, c("stock_id", "date", "Div_Yld", "Eps", "Mkt_Cap_12M_Usd", "Mom_11M_Usd", "Ocf", "Pb", "Vol1Y_Usd")]
#' svm_pred(train_data_ex, test_data_ex, config = list(kernel = "linear", cost = 1))
#' }
svm_pred <- function(train_data, test_data, config = list()) {
  # Input Validation
  checkmate::assert_data_frame(train_data, min.rows = 1, any.missing = FALSE, .var.name = "train_data")
  checkmate::assert_data_frame(test_data, min.rows = 1, any.missing = FALSE, .var.name = "test_data")
  checkmate::assert_list(config, .var.name = "config")

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
  return_label <- colnames(train_data)[3]
  test_data <- full_data$data[(nrow(train_data) + 1):nrow(full_data$data), -which(names(full_data$data) == return_label)]

  # Prepare training data
  train_features <- as.matrix(train_data[, 4:ncol(train_data)])
  train_label <- train_data[[3]]  # Assuming the 3rd column is the response variable

  # Train the SVM model using e1071::svm
  model <- tryCatch(
    e1071::svm(
      x = train_features,
      y = train_label,
      type = config$type,
      kernel = config$kernel,
      epsilon = config$epsilon,
      gamma = config$gamma,
      cost = config$cost
    ),
    error = function(e) {
      cli::cli_abort("Error training SVM model: {e$message}")
    }
  )

  # Predict using the test data
  test_features <- as.matrix(test_data[, 3:ncol(test_data)])
  predictions <- tryCatch(
    predict(model, test_features),
    error = function(e) {
      cli::cli_abort("Error during SVM prediction: {e$message}")
    }
  )

  # Ensure predictions are numeric vectors
  predictions <- as.numeric(predictions)

  # Match predictions back to stock_id and date
  predictions <- tibble::tibble(
    stock_id = test_data$stock_id,
    date = test_data$date,
    pred_return = predictions
  )

  return(predictions)
}

#' Caret Wrapper Prediction Function
#'
#' This function trains machine learning models using the caret package to predict returns based on the provided
#' training data and makes predictions for the test data. It supports various models with optional hyperparameter tuning.
#'
#' @param train_data A data frame containing `stock_id`, `date`, `return_label`, and feature columns.
#' @param test_data A data frame containing `stock_id`, `date`, and feature columns.
#' @param config A list containing caret configuration parameters, including `method`, `tuneGrid`, `trControl`, etc.
#'
#' @return A tibble with `stock_id`, `date`, and `pred_return` matching the `test_data`.
#' @importFrom caret train trainControl
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows select pull mutate_if
#' @importFrom checkmate assert_data_frame assert_list assert_string assert_flag
#' @importFrom cli cli_abort cli_alert_info
#' @export
#'
#' @examples
#' \dontrun{
#' data(data_ml)
#' train_data_ex <- data_ml[1:100, c("stock_id", "date", "R1M_Usd", "Div_Yld", "Eps",
#'                                   "Mkt_Cap_12M_Usd", "Mom_11M_Usd", "Ocf", "Pb", "Vol1Y_Usd")]
#' test_data_ex <- data_ml[101:150, c("stock_id", "date", "Div_Yld", "Eps",
#'                                   "Mkt_Cap_12M_Usd", "Mom_11M_Usd", "Ocf", "Pb", "Vol1Y_Usd")]
#' config <- list(
#'   method = "glmnet",
#'   tuneGrid = expand.grid(alpha = 0.5, lambda = 0.1),
#'   trControl = trainControl(method = "cv", number = 5),
#'   metric = "RMSE"
#' )
#' caret_predictions <- caret_wrapper(train_data_ex, test_data_ex, config)
#' }
caret_wrapper <- function(train_data, test_data, config = list()) {

  # Load necessary packages
  if (!requireNamespace("caret", quietly = TRUE)) {
    stop("Package 'caret' is required but not installed.")
  }

  # Input Validation
  checkmate::assert_data_frame(train_data, min.rows = 1, any.missing = FALSE, .var.name = "train_data")
  checkmate::assert_data_frame(test_data, min.rows = 1, any.missing = FALSE, .var.name = "test_data")
  checkmate::assert_list(config, .var.name = "config")

  # Ensure 'method' is provided in config
  if (!"method" %in% names(config)) {
    cli::cli_abort("Please provide a 'method' for caret model training in the config list.")
  }

  # Set default trControl if not provided
  if (!"trControl" %in% names(config)) {
    config$trControl <- caret::trainControl(method = "none")
  }

  # Handle missing values
  # Replace with your actual check_missing_values function
  if (any(is.na(train_data)) || any(is.na(test_data))) {
    cli::cli_abort("Data contains missing values. Please handle them before training.")
  }

  # Encode categorical data
  # Replace with your actual encode_categorical function
  full_data <- dplyr::bind_rows(train_data, test_data)
  encoded_data <- encode_categorical(full_data)

  # Split data back into training and testing sets
  train_encoded <- encoded_data$data[1:nrow(train_data), ]

  # Assuming the third column is return_label
  return_label <- colnames(train_encoded)[3]

  test_encoded <- encoded_data$data[(nrow(train_data) + 1):nrow(encoded_data$data), ]

  # Remove the return_label from test data
  test_encoded <- test_encoded %>% dplyr::select(-all_of(return_label))

  # Prepare training features and labels
  feature_cols <- setdiff(colnames(train_encoded), c("stock_id", "date", return_label))

  if (length(feature_cols) == 0) {
    cli::cli_abort("No feature columns found in train_data after encoding.")
  }

  train_features <- train_encoded %>% dplyr::select(all_of(feature_cols))
  train_label_vector <- train_encoded %>% dplyr::pull(all_of(return_label))

  # Prepare test data
  test_features <- test_encoded %>% dplyr::select(all_of(feature_cols))

  # Train the model using caret
  cli::cli_alert_info("Training the model using method: {config$method}")

  model <- tryCatch(
    suppressWarnings(caret::train(
      x = as.matrix(train_features),
      y = as.numeric(train_label_vector),
      method = config$method,
      tuneGrid = if (!is.null(config$tuneGrid)) config$tuneGrid else NULL,
      trControl = config$trControl,
      metric = if (!is.null(config$metric)) config$metric else "RMSE",
      preProcess = if (!is.null(config$preProcess)) config$preProcess else NULL
    )),
    error = function(e) {
      cli::cli_abort("Error during caret model training: {e$message}")
    }
  )

  cli::cli_alert_info("Model training completed. Proceeding to predictions...")

  # Predict using the test data
  predictions <- tryCatch(
    predict(model, newdata = as.matrix(test_features)),
    error = function(e) {
      cli::cli_abort("Error during model prediction: {e$message}")
    }
  )

  # Ensure predictions are numeric vectors
  predictions <- as.numeric(predictions)

  # Match predictions back to stock_id and date
  predictions_tibble <- tibble::tibble(
    stock_id = test_encoded$stock_id,
    date = test_encoded$date,
    pred_return = predictions
  )

  cli::cli_alert_info("Predictions generated successfully.")

  return(predictions_tibble)
}

