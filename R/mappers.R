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
