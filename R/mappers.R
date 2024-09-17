#' Return Prediction Mapping Function (Helper)
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
#' @importFrom cli cli_alert_info
#' @export
retpred_map <- function(t, data_subset, indices, model_function, model_config){
  # Input Validation
  if(!is.numeric(t) || length(t) != 1 || t < 1 || t > nrow(indices)){
    stop("Parameter 't' must be a valid row index of 'indices'.")
  }

  if(!is.character(model_function) || length(model_function) != 1){
    stop("Parameter 'model_function' must be a single character string.")
  }

  if(!model_function %in% ls(envir = globalenv())){
    stop(paste("Model function", model_function, "not found in the global environment."))
  }

  # Optional: Informative Message (handled in main functions to avoid clutter)

  # Set training and test data
  train_data <- data_subset %>%
    filter(date >= indices$training_start[t], date <= indices$training_end[t])

  test_data <- data_subset %>%
    filter(date >= indices$prediction_start[t], date < indices$prediction_end[t]) %>%
    select(-3)  # Assuming the third column is the return_label

  # Invoke training function
  training_function <- match.fun(model_function)
  predictions <- training_function(train_data, test_data, model_config)

  # Return predictions
  return(predictions)
}
