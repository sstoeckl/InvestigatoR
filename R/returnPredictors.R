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
#' ols_pred(train_data_ex, test_data_ex, config=list(), fast=TRUE)
#' ols_pred(train_data_ex, test_data_ex, config=list(), fast=FALSE)
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

