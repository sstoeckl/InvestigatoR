#' Return Prediction Mapping Function (Helper)
#'
#' This helper function is used within `backtesting_returns` to map over time indices and generate return predictions using specified ML models.
#'
#' @param t Integer. Index of the current iteration.
#' @param data_subset Tibble. Subset of the data containing `stock_id`, `date`, `return_label`, and features.
#' @param indices Tibble. Contains `training_start`, `training_end`, `prediction_start`, and `prediction_end` dates.
#' @param model_function Character. Name of the prediction (model) function to be invoked.
#' @param model_config List. Configuration parameters for the model function.
#'
#' @return Tibble containing `stock_id`, `date`, and `pred_return`.
#'
#' @importFrom dplyr filter select
#' @importFrom checkmate assert_integer assert_string assert_list
#' @importFrom cli cli_alert_danger
#'
#' @export
retpred_map <- function(t, data_subset, indices, model_function, model_config){
  # Input Validation using checkmate
  checkmate::assert_numeric(t, lower = 1, upper = nrow(indices), len = 1, .var.name = "t")
  checkmate::assert_string(model_function, .var.name = "model_function")
  checkmate::assert_list(model_config, .var.name = "model_config")

  # Resolve the model function within the package's namespace
  training_function <- tryCatch(
    match.fun(model_function),
    error = function(e) {
      cli::cli_alert_danger("Model function '{model_function}' does not exist in the package namespace.")
      stop(e)
    }
  )

  # Set training and test data
  train_data <- data_subset %>%
    dplyr::filter(date >= indices$training_start[t], date <= indices$training_end[t])
  return_label <- colnames(data_subset)[4]  # The return label is in the third
  test_data <- data_subset %>%
    dplyr::filter(date >= indices$prediction_start[t], date < indices$prediction_end[t]) %>%
    dplyr::select(-all_of(return_label))  # Assuming 'return_label' is defined in the parent scope

  # Invoke the training function
  predictions <- tryCatch(
    training_function(train_data, test_data, model_config),
    error = function(e) {
      cli::cli_alert_danger("Error in '{model_function}': {e$message}")
      stop(e)
    }
  )

  # Ensure the returned predictions have the correct structure
  if (!all(c("stock_id", "date", "pred_return") %in% colnames(predictions))) {
    cli::cli_alert_danger("Predictions from '{model_function}' do not contain the required columns: 'stock_id', 'date', 'pred_return'.")
    stop("Invalid prediction output structure.")
  }

  return(predictions)
}

#' Weight Prediction Mapping Function (Helper)
#'
#' This helper function is used within `backtesting_weights` to map over time indices and generate weight predictions using specified ML models.
#'
#' @param t Integer. Index of the current iteration.
#' @param data_subset Tibble. Subset of the data containing `stock_id`, `date`, `return_label`, features, and any optional benchmark/masking data.
#' @param indices Tibble. Contains `training_start`, `training_end`, `prediction_start`, and `prediction_end` dates.
#' @param model_function Character. Name of the weight prediction function to be invoked.
#' @param model_config List. Configuration parameters for the model function.
#'
#' @return Tibble containing `stock_id`, `date`, and predicted `weights`.
#'
#' @importFrom dplyr filter select
#' @importFrom checkmate assert_integer assert_string assert_list
#' @importFrom cli cli_alert_danger
#'
#' @examples
#' \dontrun{
#' data(data_ml)
#' # Create a subset of data_ml for testing
#' test_data_ml <- data_ml %>%
#'   filter(stock_id <= 5)
#'
#' # Define common parameters for testing
#' return_label <- "R1M_Usd"
#' features <- c("Div_Yld", "Eps", "Mkt_Cap_12M_Usd", "Mom_11M_Usd", "Ocf", "Pb", "Vol1Y_Usd")
#'
#' indices <- tibble::tibble(
#'   training_start = rep(as.Date("2012-01-01"), 2),
#'   training_end = rep(as.Date("2012-12-31"), 2),
#'   prediction_start = rep(as.Date("2013-01-01"), 2),
#'   prediction_end = rep(as.Date("2013-12-31"), 2)
#' )
#'
#' # Dummy function for weight prediction (in place of keras_weights)
#' dummy_weights_func <- function(train_data, test_data, config) {
#'   tibble::tibble(
#'     stock_id = test_data$stock_id,
#'     date = test_data$date,
#'     pred_weight = runif(nrow(test_data), -1, 1)  # Random weights for example
#'   )
#' }
#' dummy_weights_func(train_data_ml, test_data_ml, config)
#' # Use weight_map to predict weights for the specified time step
#' t <- 1  # First time index
#' weights <- weightpred_map(
#'   t = t,
#'   data_subset = test_data_ml,
#'   indices = indices,
#'   model_function = "dummy_weights_func",
#'   model_config = list()
#' )
#' print(weights)
#' }
#' @export
weightpred_map <- function(t, data_subset, indices, model_function, model_config){
  # Input Validation using checkmate
  checkmate::assert_numeric(t, lower = 1, upper = nrow(indices), len = 1, .var.name = "t")
  checkmate::assert_string(model_function, .var.name = "model_function")
  checkmate::assert_list(model_config, .var.name = "model_config")

  # Resolve the model function within the package's namespace
  weight_function <- tryCatch(
    match.fun(model_function),
    error = function(e) {
      cli::cli_alert_danger("Weight function '{model_function}' does not exist in the package namespace.")
      stop(e)
    }
  )

  # Set training and test data
  train_data <- data_subset %>%
    dplyr::filter(date >= indices$training_start[t], date <= indices$training_end[t])
  return_label <- colnames(data_subset)[4]  # The return label is in the third
  test_data <- data_subset %>%
    dplyr::filter(date >= indices$prediction_start[t], date < indices$prediction_end[t]) %>%
    dplyr::select(-all_of(return_label))  # Assuming 'return_label' is defined in the parent scope

  # Invoke the weight prediction function
  result <- tryCatch(
    weight_function(train_data, test_data, model_config),
    error = function(e) {
      cli::cli_alert_danger("Error in '{model_function}': {e$message}")
      stop(e)
    }
  )

  # Ensure the returned predictions have the correct structure
  if (!all(c("stock_id", "date", "pred_weight") %in% colnames(weights))) {
    cli::cli_alert_danger("Weights from '{model_function}' do not contain the required columns: 'stock_id', 'date', 'pred_weight'.")
    stop("Invalid weights output structure.")
  }

  # Ensure the returned predictions have the correct structure
  if (!is.list(result) || !all(c("predictions", "histories") %in% names(result))) {
    cli::cli_alert_danger("Weights from '{model_function}' do not contain the required list elements: 'predictions', 'histories'.")
    stop("Invalid weights output structure.")
  }

  # Further validate predictions
  if (!all(c("stock_id", "date", "pred_weight") %in% colnames(result$predictions))) {
    cli::cli_alert_danger("Predictions from '{model_function}' do not contain the required columns: 'stock_id', 'date', 'pred_weight'.")
    stop("Invalid predictions output structure.")
  }

  return(result$predictions)
}
