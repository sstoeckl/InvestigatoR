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
  # Encode both training and testing data to ensure consistency
  # Creating a full dataset to learn the encoding
  full_data <- bind_rows(train_data, test_data)
  # check for missing values
  check_missing_values(train_data)
  check_missing_values(test_data)
  # Applying encode_categorical on the full dataset
  encoding_results <- encode_categorical(full_data)
  encoded_data <- encoding_results$data
  # Splitting the encoded data back into training and testing sets
  train_data <- encoded_data[1:nrow(train_data), ]
  test_data <- encoded_data[(nrow(train_data) + 1):nrow(encoded_data), ]
  # now lets do the prediction
  if (fast) {
    # fast cpp implementation
    mm <- cbind(1, as.matrix(train_data[,4:ncol(train_data)]))   # model matrix
    y  <- train_data |> dplyr::pull(3)            # response
    plm <- RcppArmadillo::fastLm(mm, y)
    predictions <- predict(plm, cbind(1, as.matrix(test_data[,3:ncol(test_data)])))
  } else {
    # standard lm implementation
    label <- colnames(train_data)[3]
    formula_lm <- as.formula(paste0(colnames(train_data)[3], " ~ ."))
    flm <- lm(formula_lm, data=train_data[,3:ncol(train_data)])
    predictions <- as.vector(predict(flm, test_data[,3:ncol(test_data)]))
  }
  # match preds back to stock_id and date
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
  if (!"alpha" %in% names(config)) {
    config$alpha <- 0.5  # Mix between Ridge (0) and Lasso (1)
  }
  if (!"lambda" %in% names(config)) {
    config$lambda <- 0.01  # Regularization strength
  }
  # Encode both training and testing data to ensure consistency
  # Creating a full dataset to learn the encoding
  full_data <- bind_rows(train_data, test_data)
  # check for missing values
  check_missing_values(train_data)
  check_missing_values(test_data)
  # Applying encode_categorical on the full dataset
  encoding_results <- encode_categorical(full_data)
  encoded_data <- encoding_results$data
  # Splitting the encoded data back into training and testing sets
  train_data <- encoded_data[1:nrow(train_data), ]
  test_data <- encoded_data[(nrow(train_data) + 1):nrow(encoded_data), ]
  # Prepare the model matrix and response vector
  x_train <- as.matrix(train_data[, -(1:3)])  # Exclude non-feature columns
  y_train <- train_data[[which(names(train_data) == "return_label")]]

  # Train the Elastic Net model
  enet_model <- glmnet(x_train, y_train, alpha = config$alpha, lambda = config$lambda)

  # Prepare test data and predict
  x_test <- as.matrix(test_data[, -(1:3)])
  predictions <- predict(enet_model, s = config$lambda, newx = x_test)

  # Match predictions back to stock_id and date
  predictions <- tibble::tibble(stock_id = test_data$stock_id, date = test_data$date, pred_return = predictions)

  return(predictions)
}

#' xgb function for return prediction
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
xgb_pred <- function(train_data, test_data, config) {
  # Default parameters for xgb.train (simplified example)
  default_params <- list(
    eta = 0.3,
    max_depth = 4,
    #gamma = 0,
    objective = "reg:squarederror",
    nrounds = 80
  )

  # Function to check and add missing arguments
  ensure_config <- function(config, default_params) {
    missing_args <- setdiff(names(default_params), names(config))
    if (length(missing_args) > 0) {
      message("Adding default values for missing arguments: ", paste(missing_args, collapse=", "))
      for (arg in missing_args) {
        config[[arg]] <- default_params[[arg]]
      }
    }
    return(config)
  }
  ## check config
  config <- ensure_config(config, default_params)

  # Encode both training and testing data to ensure consistency
  # Creating a full dataset to learn the encoding
  full_data <- bind_rows(train_data, test_data)
  # check for missing values
  check_missing_values(train_data)
  check_missing_values(test_data)
  # Applying encode_categorical on the full dataset
  encoding_results <- encode_categorical(full_data)
  encoded_data <- encoding_results$data
  # Splitting the encoded data back into training and testing sets
  train_data <- encoded_data[1:nrow(train_data), ]
  test_data <- encoded_data[(nrow(train_data) + 1):nrow(encoded_data), ]

  # xgboost requires the data to be in a specific format
  train_features <- as.matrix(train_data[,4:ncol(train_data)])
  train_label <- as.matrix(train_data[,3])
  train_matrix <- xgboost::xgb.DMatrix(data = train_features, label = train_label)   # XGB format
  # add data
  config$data <- train_matrix
  # do the training
  fit <- do.call(xgboost::xgb.train, config)
  # do the predictions
  xgb_test <- as.matrix(test_data[,3:ncol(test_data)])  |> xgboost::xgb.DMatrix()
  predictions <- as.vector(predict(fit, xgb_test))

  # match preds back to stock_id and date
  predictions <- tibble::tibble(stock_id=test_data$stock_id, date=test_data$date, pred_return=predictions)
  return(predictions)
}
#' random forest function for return prediction
#'
#' @param train_data data frame with stock_id, date, return_label and features
#' @param test_data data frame with stock_id, date and features
#' @param config empty list, as ols does not need any configuration
#' @param fast logical, if TRUE, use fastLm from RcppArmadillo, else use lm from base R
#'
#' @return tibble with stock_id, date and pred_return matching the test_data
#'
#' @import ranger
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
#' rf_pred(train_data_ex, test_data_ex, config=list())
#' }
#'
rf_pred <- function(train_data, test_data, config) {
  # Default parameters for ranger
  default_params <- list(
    num.trees = 40,                # Nb of random trees
    mtry = 3                  # Nb of variables randomly sampled as candidates at each split
  )

  # Function to merge default and user-supplied configurations
  ensure_config <- function(config, default_params) {
    for (arg in names(default_params)) {
      if (!arg %in% names(config)) {
        config[[arg]] <- default_params[[arg]]
      }
    }
    return(config)
  }

  # Merge user config with default config
  config <- ensure_config(config, default_params)

  # Encode both training and testing data to ensure consistency
  # Creating a full dataset to learn the encoding
  full_data <- bind_rows(train_data, test_data)
  # check for missing values
  check_missing_values(train_data)
  check_missing_values(test_data)
  # Applying encode_categorical on the full dataset
  encoding_results <- encode_categorical(full_data)
  encoded_data <- encoding_results$data
  # Splitting the encoded data back into training and testing sets
  train_data <- encoded_data[1:nrow(train_data), ]
  test_data <- encoded_data[(nrow(train_data) + 1):nrow(encoded_data), ]

  # Prepare training features and labels
  train_features <- as.matrix(train_data[,4:ncol(train_data)])
  train_label <- as.matrix(train_data[,3])  # Assuming the 3rd column is the response variable

  # Configure and train the random forest model
  # config$formula <- as.formula(paste0(colnames(train_data)[3], " ~ ."))
  # config$data <- train_data  # Including the entire data might be necessary for some configs
  config$y <- train_label
  config$x <- train_features
  fit <- do.call(ranger::ranger, config)

  # Predict on the test data
  test_features <- test_data[,3:ncol(test_data)]
  predictions <- predict(fit, data = test_features)$predictions

  # match preds back to stock_id and date
  predictions <- tibble::tibble(stock_id=test_data$stock_id, date=test_data$date, pred_return=predictions)
  return(predictions)
}
#' Support Vector Machine (SVM) Prediction Function
#'
#' Trains a Support Vector Machine using the e1071 package and predicts outcomes
#' for the provided test data set based on the trained model.
#'
#' @param train_data Data frame containing training data with features and a target variable.
#' @param test_data Data frame containing test data with features.
#' @param config List containing any additional parameters for the SVM model.
#'
#' @return A tibble with stock_id, date, and predicted returns.
#'
#' @importFrom e1071 svm
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
#' svm_pred(train_data_ex, test_data_ex, config)
#' }
svm_pred <- function(train_data, test_data, config = list()) {
  library(e1071)

  # Default parameters
  default_params <- list(type = "eps-regression",          # SVM task type (see LIBSVM documentation)
                         kernel = "radial",                # SVM kernel (or: linear, polynomial, sigmoid)
                         epsilon = 0.1,                    # Width of strip for errors
                         gamma = 0.5,                      # Constant in the radial kernel
                         cost = 0.1)                       # Slack variable penalisation

  # Ensure config
  ensure_config <- function(config, default_params) {
    for (arg in names(default_params)) {
      if (!arg %in% names(config)) {
        config[[arg]] <- default_params[[arg]]
      }
    }
    return(config)
  }
  config <- ensure_config(config, default_params)
  # Encode both training and testing data to ensure consistency
  # Creating a full dataset to learn the encoding
  full_data <- bind_rows(train_data, test_data)
  # check for missing values
  check_missing_values(train_data)
  check_missing_values(test_data)
  # Applying encode_categorical on the full dataset
  encoding_results <- encode_categorical(full_data)
  encoded_data <- encoding_results$data
  # Splitting the encoded data back into training and testing sets
  train_data <- encoded_data[1:nrow(train_data), ]
  test_data <- encoded_data[(nrow(train_data) + 1):nrow(encoded_data), ]
  # Prep data for svm
  train_features <- as.matrix(train_data[,4:ncol(train_data)])
  train_label <- as.matrix(train_data[,3])

  model <- svm(y=train_label, x=train_features, type = config$type, kernel = config$kernel,
               epsilon = config$epsilon, gamma = config$gamma, cost = config$cost)

  # do the predictions
  test_features <- as.matrix(test_data[,3:ncol(test_data)])
  predictions <- predict(model, test_features)
  return(tibble::tibble(stock_id = test_data$stock_id, date = test_data$date, pred_return = predictions))
}
#' Neural Network Prediction Function
#'
#' Trains a neural network using the nnet package and makes predictions
#' for the test data set.
#'
#' @param train_data Data frame containing training data with features and a target variable.
#' @param test_data Data frame containing test data with features.
#' @param config List containing parameters for the neural network, such as size and decay.
#'
#' @return A tibble with stock_id, date, and predicted returns.
#'
#' @importFrom nnet nnet
#' @importFrom tibble tibble
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data(data_ml)
#' train_data_ex <- data_ml[1:100,]
#' test_data_ex <- data_ml[101:150,]
#' config <- list(size = 5, decay = 0.1)
#' nnet_pred(train_data_ex, test_data_ex, config)
#' }
#' Neural Network (nnet) function for return prediction
#'
#' @param train_data data frame with stock_id, date, return_label, and features
#' @param test_data data frame with stock_id, date, and features
#' @param config list with neural network parameters (size, decay)
#'
#' @return tibble with stock_id, date, and pred_return matching the test_data
#'
#' @importFrom nnet nnet
#' @importFrom tibble tibble
#' @importFrom stats predict
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data(data_ml)
#' train_data_ex <- data_ml[1:100, c(1,2,96,5:10)]
#' test_data_ex <- data_ml[101:150, c(1,2,5:10)]
#' nnet_pred(train_data_ex, test_data_ex, config = list(size = 5, decay = 0.1))
#' }
nnet_pred <- function(train_data, test_data, config) {
  # Default parameters for nnet
  default_params <- list(
    size = 5,
    decay = 0.1
  )

  # Function to check and add missing arguments
  ensure_config <- function(config, default_params) {
    missing_args <- setdiff(names(default_params), names(config))
    if (length(missing_args) > 0) {
      message("Adding default values for missing arguments: ", paste(missing_args, collapse=", "))
      for (arg in missing_args) {
        config[[arg]] <- default_params[[arg]]
      }
    }
    return(config)
  }

  # Check config
  config <- ensure_config(config, default_params)

  # Encode both training and testing data to ensure consistency
  # Creating a full dataset to learn the encoding
  full_data <- bind_rows(train_data, test_data)
  # check for missing values
  check_missing_values(train_data)
  check_missing_values(test_data)
  # Applying encode_categorical on the full dataset
  encoding_results <- encode_categorical(full_data)
  encoded_data <- encoding_results$data
  # Splitting the encoded data back into training and testing sets
  train_data <- encoded_data[1:nrow(train_data), ]
  test_data <- encoded_data[(nrow(train_data) + 1):nrow(encoded_data), ]

  # Prep data
  train_features <- as.matrix(train_data[,4:ncol(train_data)])
  train_label <- as.matrix(train_data[,3])

  # Train neural network model
  model <- nnet::nnet(y = train_label, x = train_features, size = config$size, decay = config$decay, linout = TRUE)

  # Predict using the test data
  test_features <- as.matrix(test_data[,3:ncol(test_data)])
  predictions <- as.vector(predict(model, test_features[, 3:ncol(test_features)]))

  # Match predictions to stock_id and date
  predictions <- tibble::tibble(stock_id = test_data$stock_id, date = test_data$date, pred_return = predictions)

  return(predictions)
}
