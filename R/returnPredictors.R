#' ols function for return prediction
#'
#' @param train_data data frame with stock_id, date, return_label and features
#' @param test_data data frame with stock_id, date and features
#' @param config empty list, as ols does not need any configuration
#' @param fast logical, if TRUE, use fastLm from RcppArmadillo, else use lm from base R
#'
#' @return tibble with stock_id, date and pred_return matching the test_data
#'
#' @importFrom RcppArmadillo fastLm
#' @importFrom dplyr pull
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
#' ols_pred(train_data_ex, test_data_ex, config=list(), fast=TRUE)
#' ols_pred(train_data_ex, test_data_ex, config=list(), fast=FALSE)
#' }
ols_pred <- function(train_data, test_data, config=list(), fast=TRUE) {
  # Handle missing values
  check_missing_values(train_data)
  check_missing_values(test_data)

  # Encode categorical data
  full_data <- bind_rows(train_data, test_data)
  full_data <- encode_categorical(full_data)

  # Split data back
  train_data <- full_data$data[1:nrow(train_data), ]
  test_data <- full_data$data[(nrow(train_data) + 1):nrow(full_data$data), -3]

  # Perform OLS prediction
  if (fast) {
    # Fast version with RcppArmadillo
    mm <- cbind(1, as.matrix(train_data[,4:ncol(train_data)]))
    y  <- train_data |> dplyr::pull(3)
    plm <- RcppArmadillo::fastLm(mm, y)
    predictions <- predict(plm, cbind(1, as.matrix(test_data[,3:ncol(test_data)])))
  } else {
    # Standard version with lm
    formula_lm <- as.formula(paste0(colnames(train_data)[3], " ~ ."))
    flm <- lm(formula_lm, data=train_data[,3:ncol(train_data)])
    predictions <- as.vector(predict(flm, test_data[,3:ncol(test_data)]))
  }

  # Return predictions
  predictions <- tibble::tibble(stock_id=test_data$stock_id, date=test_data$date, pred_return=predictions)
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

#' Random Forest function for return prediction
#'
#' This function trains a Random Forest model to predict returns based on the provided training data
#' and predicts returns for the test data.
#'
#' @param train_data A data frame with stock_id, date, return_label, and features.
#' @param test_data A data frame with stock_id, date, and features.
#' @param config A list of random forest configuration parameters (e.g., num.trees, mtry).
#'
#' @return A tibble with stock_id, date, and predicted returns matching the test data.
#' @import ranger
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows
#' @export
#'
#' @examples
#' \dontrun{
#' data(data_ml)
#' train_data_ex <- data_ml[1:100, c(1,2,96,5:10)]
#' test_data_ex <- data_ml[101:150, c(1,2,5:10)]
#' rf_pred(train_data_ex, test_data_ex, config = list(num.trees = 200, mtry = 4))
#' }
rf_pred <- function(train_data, test_data, config = list()) {
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
  test_data <- full_data$data[(nrow(train_data) + 1):nrow(full_data$data), -3]

  # Prepare training features and labels
  train_features <- as.matrix(train_data[, 4:ncol(train_data)])
  train_label <- as.matrix(train_data[, 3])  # Assuming the 3rd column is the response variable

  # Train the random forest model using ranger
  fit <- ranger::ranger(
    x = train_features,
    y = train_label,
    num.trees = config$num.trees,
    mtry = config$mtry
  )

  # Prepare test data and predict
  test_features <- as.matrix(test_data[, 3:ncol(test_data)])
  predictions <- predict(fit, data = test_features)$predictions

  # Match predictions back to stock_id and date
  predictions <- tibble::tibble(stock_id = test_data$stock_id, date = test_data$date, pred_return = predictions)

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
