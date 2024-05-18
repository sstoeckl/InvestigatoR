#' This function is for backtesting portfolios given return predictions
#'
#' @param return_prediction_object a returnPrediction S3 object
#' @param portfolio_object a portfolio S3 object
#' @param pf_config a list of configurations for the portfolio
#' @param append boolean, whether to append the portfolio to the portfolio object
#'
#' @return a portfolio object suitable for furtehr processing with summary or plot
#'
#' @importFrom dplyr select all_of arrange group_by mutate ungroup
#'
#' @export
#'
#' @examples
#' \dontrun{
#' pf_config <- list(predictions = c("ols_1","xgb_1","xgb_2"),
#'                  quantile_weight = list(pred_func="quantile_weights",
#'                  config1=list(quantiles = list(long = 0.20, short = 0.20),allow_short_sale = TRUE,
#'                  min_weight = -1,  max_weight = 1, b = 1),
#'                  config2=list(quantiles = list(long = 0.10, short = 0.10),allow_short_sale = TRUE,
#'                  min_weight = -1,  max_weight = 1, b = 1)))
#' }
backtesting_portfolios <- function(return_prediction_object, portfolio_object = NULL, pf_config, append = FALSE) {
  # Initialize the portfolio object if not supplied
  if (is.null(portfolio_object)) {
    portfolio_object <- create_portfolios(return_prediction_object$actual_returns, "actual_return")
  }

  # Loop through each model specified in pf_config
  model_ids <- pf_config$predictions
  specific_predictions <- return_prediction_object$predictions %>%
    dplyr::select(stock_id, date, dplyr::all_of(model_ids))
  specific_errors <- return_prediction_object$errors %>%
    dplyr::select(stock_id, date, dplyr::all_of(model_ids))

  # Loop through different weight configurations
  for (i in 2:length(pf_config)) {
    cat("Currently processing weight model", i-1, "of", length(pf_config)-1, "\n")
    # extract model name and config
    model_name <- names(pf_config)[i]

    for (j in 2:length(pf_config[[model_name]])){
      cat("Specifically processing config", j-1, "of", length(pf_config[[model_name]])-1, "\n")
      weight_specs <- pf_config[[model_name]]
      # extract model function and config
      weight_model <- weight_specs$pred_func
      weight_config <- weight_specs[[j]]

      # Dynamically call the appropriate weight function
      training_function <- match.fun(weight_model)
      new_weights <- training_function(specific_predictions, errors, weight_config)
      colnames(new_weights)[-c(1:2)] <- model_ids

      for (col in 3:ncol(new_weights)) {
        new_weights_ind <- new_weights |>  dplyr::select(stock_id, date, all_of(col))
        # Store or update the weights in the portfolio object
        portfolio_object <- add_weight_model(portfolio_object, weight_model, weight_config, new_weights_ind)
      }

    }
  }
  return(portfolio_object)
}


