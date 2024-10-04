#' Backtesting Portfolio Weights
#'
#' This function predicts portfolio weights directly using neural networks and other models.
#' It can apply custom loss functions that maximize financial performance, such as returns,
#' Sharpe ratio, or information ratio, while respecting constraints like turnover or leverage.
#'
#' @param data Data frame in long format containing stock IDs, dates, and features.
#' @param return_label Column name of the actual return.
#' @param features Character vector of feature names used for weight prediction.
#' @param pf_config List of portfolio configuration options, including model configurations.
#' @param portfolio_object An existing portfolioReturns object to append results to (default: NULL).
#' @param rolling Logical indicating whether to use a rolling window approach.
#' @param window_size Character string specifying the window size (e.g., "5 years").
#' @param step_size Character string specifying the step size for the rolling window (e.g., "1 month").
#' @param offset Character string specifying the offset to avoid look-ahead bias.
#' @param in_sample Logical indicating whether to provide in-sample predictions.
#' @param num_cores Number of cores to use for parallel processing.
#' @param verbose Logical indicating whether to display detailed progress messages.
#'
#' @return An S3 object of class `portfolioReturns` containing the predicted weights and performance metrics.
#' @importFrom dplyr bind_rows
#' @importFrom future plan multicore sequential
#' @importFrom furrr future_map_dfr
#' @importFrom tibble tibble
#' @import checkmate
#'
#' @examples
#' \dontrun{
#' data(data_ml)
#' # Create a subset of data_ml for testing
#' test_data_ml <- data_ml %>%
#'   filter(stock_id <= 5)
#'
#' return_label <- "R1M_Usd"
#' features <- c("Div_Yld", "Eps", "Mkt_Cap_12M_Usd", "Mom_11M_Usd", "Ocf", "Pb", "Vol1Y_Usd")
#'
#' # Dummy weight function example (replace with your real model function)
#' dummy_weights_func <- function(train_data, test_data, config) {
#'   tibble::tibble(
#'     stock_id = test_data$stock_id,
#'     date = test_data$date,
#'     pred_weight = runif(nrow(test_data), -1, 1)  # Random weights for example
#'   )
#' }
#'
#' pf_config <- list(
#'   predictions = c("dummy_weights_func"),
#'   dummy_weights_func = list(list())
#' )
#'
#' window_size <- "5 years"
#' step_size <- "1 year"
#' offset <- "1 month"
#' in_sample <- TRUE
#' num_cores <- 2
#'
#' # Perform backtesting
#' portfolio <- backtesting_weights(
#'   data = test_data_ml,
#'   return_label = return_label,
#'   features = features,
#'   pf_config = pf_config,
#'   rolling = FALSE,
#'   window_size = window_size,
#'   step_size = step_size,
#'   offset = offset,
#'   in_sample = in_sample,
#'   num_cores = num_cores,
#'   verbose = TRUE
#' )
#' print(portfolio)
#' }
#' @export
backtesting_weights <- function(data, return_label, features, pf_config,
                                portfolio_object = NULL,
                                rolling = TRUE, window_size = "5 years", step_size = "1 month",
                                offset = "1 month", in_sample = TRUE, num_cores = NULL, verbose = FALSE) {

  # Input validation
  checkmate::assert_data_frame(data, min.rows = 1, any.missing = FALSE)
  checkmate::assert_character(features, min.len = 1)
  checkmate::assert_list(pf_config)

  # If no portfolio_object is provided, create a new one
  if (is.null(portfolio_object)) {
    portfolio_object <- create_portfolios(data, return_label)
  }

  # Extract model configuration
  pred_funcs <- pf_config$predictions

  # Prepare data for weight prediction
  dates <- data %>% dplyr::distinct(date) %>% dplyr::arrange(date) %>% dplyr::pull(date)
  indices <- select_dates_by_offset(dates, window_size, step_size, offset, rolling)

  # Subset data to include only necessary columns
  data_subset <- data %>%
    dplyr::select(stock_id, date, dplyr::all_of(return_label), dplyr::all_of(features))

  # Initialize result containers
  all_weights <- list()

  # Set up parallel processing if specified
  if (!is.null(num_cores)) {
    future::plan(multicore, workers = num_cores)
  } else {
    future::plan("sequential")
  }

  # Loop through each model in pf_config and predict weights
  for (pred_func_name in pred_funcs) {
    pred_func <- get(pred_func_name)  # Get the function by name
    config <- pf_config[[pred_func_name]]

    for (i in seq_along(config)) {
      model_config <- config[[i]]

      if (verbose) {
        message("Running prediction for model: ", pred_func_name, " (Config ", i, ")")
      }

      # Map over rolling windows to generate predictions
      weight_predictions <- furrr::future_map_dfr(
        seq_len(nrow(indices)),
        ~ {
          result <- weightpred_map(.x, data_subset, indices, pred_func_name, model_config)
          return(result)
        },
        .options = furrr::furrr_options(seed = TRUE)
      )

      # Add the weights to the portfolio object
      portfolio_object <- add_weight_model(portfolio_object, pred_func_name, model_config, weight_predictions)
    }
  }

  future::plan("sequential")  # Reset to sequential processing

  return(portfolio_object)
}
