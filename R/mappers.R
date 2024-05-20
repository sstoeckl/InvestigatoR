#' @title Return Prediction Mapping Function (Helper)
#'
#' @param t integer, index of the current iteration
#' @param data_subset tibble, subset of the data (stock_id, date, return_label, features)
#' @param indices list, list of training and prediction indices
#' @param model_function character, name of the prediction (model) function
#' @param model_config list, configuration for the model
#'
#' @return tibble that contains stock id, date, and predicted return
#'
#' @importFrom dplyr filter select
#'
#' @export
#'
retpred_map <- function(t, data_subset, indices, model_function, model_config){
  # set training and test data
  train_data <- data_subset |> dplyr::filter(date >= indices$training_start[t],   # Training set
                                date <= indices$training_end[t])
  test_data <- data_subset |> dplyr::filter(date >= indices$prediction_start[t],   # Test set
                               date < indices$prediction_end[t]) |>
    dplyr::select(-3)
  # invoke training function
  training_function <- match.fun(model_function)
  predictions <- training_function(train_data, test_data, model_config)
}
