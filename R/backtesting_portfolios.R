#' Backtesting Portfolios Function
#'
#' This function backtests portfolios based on return predictions, implementing various weight creation strategies and constraints.
#'
#' @param return_prediction_object An object of class `returnPrediction` containing return predictions.
#' @param portfolio_object An object of class `portfolio` to store portfolio returns. Default is `NULL`, which creates a new object.
#' @param pf_config A list of configurations for portfolio formation. It should specify prediction columns and weight creation strategies.
#' @param append If `TRUE`, the function will append the portfolio to the portfolio object. If `FALSE`, the object will be overwritten.
#' @param verbose If `TRUE`, detailed messages will be printed. Default is `TRUE`.
#'
#' @return An S3 object of class `portfolio` containing portfolio returns and associated information.
#'
#' @importFrom dplyr select arrange group_by mutate ungroup
#' @importFrom tibble tibble
#' @importFrom checkmate assert_class assert_list assert_flag
#' @importFrom purrr map reduce
#' @import cli
#'
#' @export
#'
#' @examples
#' \dontrun{
#' pf_config <- list(
#'   predictions = c("ols_1", "xgb_1", "xgb_2"),
#'   quantile_weight = list(
#'     pred_func = "quantile_weights",
#'     config1 = list(
#'       quantiles = list(long = 0.20, short = 0.20),
#'       allow_short_sale = FALSE,
#'       min_weight = 0,
#'       max_weight = 1,
#'       b = 1
#'     ),
#'     config2 = list(
#'       quantiles = list(long = 0.10, short = 0.10),
#'       allow_short_sale = FALSE,
#'       min_weight = 0,
#'       max_weight = 1,
#'       b = 1
#'     )
#'   )
#' )
#' pf <- backtesting_portfolios(
#'   return_prediction_object = rp,
#'   portfolio_object = NULL,
#'   pf_config = pf_config,
#'   append = FALSE,
#'   verbose = TRUE
#' )
#' }
backtesting_portfolios <- function(return_prediction_object,
                                   portfolio_object = NULL,
                                   pf_config,
                                   append = FALSE,
                                   verbose = TRUE) {

  # Input Validation
  checkmate::assert_class(return_prediction_object, classes = "returnPrediction")
  checkmate::assert_list(pf_config, types = "list")
  checkmate::assert_flag(append)
  checkmate::assert_flag(verbose)

  # Load required packages
  library(dplyr)
  library(tibble)
  library(checkmate)
  library(purrr)
  library(cli)

  # Initialize verbose messaging
  if (verbose) {
    cli::cli_inform("Starting backtesting_portfolios...")
  }

  # Create portfolio_object if not provided
  if (is.null(portfolio_object)) {
    cli::cli_inform("Creating a new portfolio object.")
    # Assuming create_portfolios is a helper function that initializes a portfolio object
    portfolio_object <- create_portfolios(return_prediction_object$actual_returns, "actual_return")
  }

  # Extract specific predictions based on pf_config
  model_ids <- pf_config$predictions
  if (!all(model_ids %in% colnames(return_prediction_object$predictions))) {
    missing_preds <- setdiff(model_ids, colnames(return_prediction_object$predictions))
    cli::cli_abort("The following predictions are missing in return_prediction_object: {paste(missing_preds, collapse = ', ')}")
  }

  specific_predictions <- return_prediction_object$predictions %>%
    select(stock_id, date, all_of(model_ids))

  specific_errors <- return_prediction_object$errors %>%
    select(stock_id, date, all_of(model_ids))

  # Loop through different weight configurations
  weight_models <- pf_config[names(pf_config) != "predictions"]

  for (i in seq_along(weight_models)) {
    weight_model_name <- names(weight_models)[i]
    weight_specs <- weight_models[[i]]

    # Extract weight function and its configurations
    weight_func <- weight_specs$pred_func
    weight_configs <- weight_specs[names(weight_specs) != "pred_func"]

    cli::cli_alert_info("Processing weight model {i}/{length(weight_models)}: {weight_model_name} using function {weight_func}")

    for (j in seq_along(weight_configs)) {
      config_name <- names(weight_configs)[j]
      config <- weight_configs[[j]]

      cli::cli_alert_info("  Processing config {j}/{length(weight_configs)}: {config_name}")

      # Check if weight_func exists
      if (!exists(weight_func, mode = "function")) {
        cli::cli_alert_danger("Weight function '{weight_func}' does not exist.")
        next  # Skip to next configuration
      }

      # Dynamically call the appropriate weight function with error handling
      new_weights <- tryCatch({
        get(weight_func)(specific_predictions, specific_errors, config)
      }, error = function(e) {
        cli::cli_alert_danger("Error in weight function '{weight_func}' with config '{config_name}': {e$message}")
        NULL
      })

      # Proceed if weights were successfully created
      if (!is.null(new_weights)) {
        # Rename weight columns to include model and config identifiers if necessary
        # Here, assuming that weight functions return 'stock_id', 'date', and 'weight' columns
        # If multiple weight columns are returned, adjust accordingly
        # For simplicity, assuming one weight column per config
        weight_col_name <- paste0(weight_model_name, "_", config_name)
        new_weights <- new_weights %>%
          rename(weight = weight) %>%
          mutate(weight_model = weight_model_name, config = config_name)

        # Add weight model to portfolio_object with error handling
        tryCatch({
          portfolio_object <- add_weight_model(portfolio_object, weight_model_name, config, new_weights)
          cli::cli_alert_success("  Successfully added weights for config '{config_name}'.")
        }, error = function(e) {
          cli::cli_alert_danger("Failed to add weights for config '{config_name}': {e$message}")
        })
      }

      # Update progress
      if (verbose) {
        cli::cli_progress_update()
      }
    }
  }

  # Final Message
  if (verbose) {
    cli::cli_inform("Completed backtesting_portfolios.")
  }

  # Return the portfolio_object
  return(portfolio_object)
}
