#' Title
#'
#' @param data xy
#' @param labels xy
#' @param features xy
#'
#' @return xy
#' @export
#'
#' @examples xy
ols_predictor <- function(data,labels, features) {
  data_seperation(data = data, labels = labels, features = features, start_date = data$date[1], end_date = data$date[nrows(data)], seperation_date= data$date[nrows(data)*0.7])
  formula <- as.formula(paste(labels, "~", paste(features, collapse = " + ")))
  model <- lm(formula, data = data)
  predictions <- predict(model, testing_sample)
  return(predictions)
}
