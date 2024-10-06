#' Backtesting Portfolio Weights
#'
#' This function predicts portfolio weights directly using neural networks and other models.
#' It applies custom loss functions while respecting constraints like turnover or leverage.
#'
#' @param data Data frame in long format containing stock IDs, dates, and features.
#' @param return_label Column name of the actual return.
#' @param benchmark_weights Column name of the benchmark weights (optional).
#' @param mask_label Column name of the mask indicator (optional).
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
#' @importFrom dplyr bind_rows distinct arrange pull mutate rename select all_of
#' @importFrom future plan multicore sequential
#' @importFrom furrr future_map_dfr
#' @importFrom tibble tibble
#' @import checkmate
#' @importFrom cli cli_inform cli_alert_info cli_alert_success cli_alert_warning cli_progress_bar cli_progress_update cli_progress_done
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
#'   dummy_weights = list(
#'     weight_func = "dummy_weights_func",
#'     config1 = list(min_weight = -0.5, max_weight = 0.5),
#'     config2 = list(min_weight = -0.5, max_weight = 0.5))
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
backtesting_weights <- function(data, return_label, benchmark_weights = NULL, mask_label = NULL, features, pf_config,
                                portfolio_object = NULL,
                                rolling = TRUE, window_size = "5 years", step_size = "1 month",
                                offset = "1 month", in_sample = TRUE, num_cores = NULL, verbose = FALSE) {

  # Input Validation using checkmate
  if (verbose) {
    cli::cli_inform("Starting input validation...")
  }

  checkmate::assert_data_frame(data, min.rows = 1, any.missing = FALSE, .var.name = "data")
  checkmate::assert_string(return_label, .var.name = "return_label")
  checkmate::assert_character(features, min.len = 1, .var.name = "features")
  checkmate::assert_list(pf_config, .var.name = "pf_config")

  if (!is.null(benchmark_weights)) {
    checkmate::assert_string(benchmark_weights, .var.name = "benchmark_weights")
  }

  if (!is.null(mask_label)) {
    checkmate::assert_string(mask_label, .var.name = "mask_label")
  }

  if (!is.null(num_cores)) {
    checkmate::assert_integerish(num_cores, lower = 1, .var.name = "num_cores")
  }

  if (verbose) {
    cli::cli_alert_success("Input validation passed.")
  }

  # If no portfolio_object is provided, create a new one
  if (is.null(portfolio_object)) {
    portfolio_object <- create_portfolios(data, return_label)
    if (verbose) {
      cli::cli_alert_info("Created new portfolio object.")
    }
  }

  # Prepare data for post-processing
  if (is.null(mask_label)) {
    data_post <- data %>% dplyr::mutate(mask = 1) %>%
      dplyr::select(stock_id, date, !!benchmark_weights, dplyr::all_of(return_label), dplyr::all_of(features), mask)
  } else {
    data_post <- data %>% dplyr::rename(mask = !!mask_label) %>%
      dplyr::select(stock_id, date, !!benchmark_weights, dplyr::all_of(return_label), dplyr::all_of(features), mask)
  }

  # Prepare data to include only necessary columns
  if (is.null(benchmark_weights)) {
    data_subset <- data_post %>% dplyr::mutate(benchmark_weights = 0) %>%
      dplyr::select(stock_id, date, benchmark_weights, dplyr::all_of(return_label), dplyr::all_of(features))
  } else {
    data_subset <- data %>%
      dplyr::select(stock_id, date, benchmark_weights = !!benchmark_weights, dplyr::all_of(return_label), dplyr::all_of(features))
  }

  # Extract dates and set rolling window indices
  dates <- data %>% dplyr::distinct(date) %>% dplyr::arrange(date) %>% dplyr::pull(date)
  indices <- select_dates_by_offset(dates, window_size, step_size, offset, rolling)

  # Set up parallel processing if specified
  if (!is.null(num_cores)) {
    future::plan(multicore, workers = num_cores)
    options(future.seed = TRUE)
    if (verbose) {
      cli::cli_alert_info("Using {num_cores} cores for parallel processing.")
    }
  } else {
    future::plan("sequential")
    if (verbose) {
      cli::cli_alert_info("Using sequential processing.")
    }
  }

  # Loop through each model in pf_config
  for (i in seq_along(pf_config)) {
    model_name <- names(pf_config)[i]
    model_specs <- pf_config[[i]]

    # Validate presence of 'pred_func'
    if (is.null(model_specs$weight_func)) {
      cli::cli_abort("Model '{model_name}' does not have a 'weight_func' specified in 'pf_config'.")
    }

    model_function <- model_specs$weight_func
    config_names <- setdiff(names(model_specs), "weight_func")

    if (length(config_names) == 0) {
      cli::cli_alert_warning("Model '{model_name}' does not have any configurations. Skipping.")
      next
    }

    cli::cli_alert_info("Processing model {i}/{length(pf_config)}: {model_name}")

    # Loop through each configuration for the current model
    for (j in seq_along(config_names)) {
      config_name <- config_names[j]
      model_config <- model_specs[[config_name]]

      cli::cli_alert_info("  Processing configuration {j}/{length(config_names)}: {config_name}")
      cli::cli_alert_info("    Using prediction function: {model_function}")

      # Define mapping indices
      map_indices <- seq_len(nrow(indices))

      # Initialize progress bar
      if (verbose) {
        pb <- cli::cli_progress_bar("    Running predictions for {model_name} - {config_name}", total = length(map_indices), clear = FALSE, format = "{cli::pb_spin} {cli::pb_percent} [{cli::pb_bar}] {cli::pb_eta}")
      }

      # Execute predictions with progress updates
      weight_predictions <- furrr::future_map_dfr(
        map_indices,
        ~ {
          result <- weightpred_map(.x, data_subset, indices, model_function, model_config)
          if (verbose) {
            cli::cli_progress_update(id = pb)
          }
          return(result)
        },
        .options = furrr::furrr_options(seed = TRUE)
      )

      # Finish progress bar
      if (verbose) {
        cli::cli_progress_done(id = pb)
      }

      # Post-process the weights to handle constraints and masking
      final_weights <- postprocess_weights(weight_predictions, data_post, model_config)

      # Add the weights to the portfolio object
      portfolio_object <- add_weight_model(portfolio_object, model_name, model_config, final_weights |> dplyr::select(-mask))

      if (verbose) {
        cli::cli_alert_success("    Successfully added weights for configuration '{config_name}' of model '{model_name}'.")
      }
    }
  }

  if (verbose) {
    cli::cli_alert_success("Completed all model processing.")
  }

  # Reset to sequential processing
  future::plan("sequential")
  if (verbose) {
    cli::cli_alert_info("Parallel processing plan reset to sequential.")
  }

  return(portfolio_object)
}


#' Postprocess Weights (Masking and Box Constraints)
#'
#' This function applies the masking and box constraints (min and max weight constraints) to the predicted portfolio weights.
#'
#' @param weight_predictions Tibble. A data frame containing stock IDs, dates, and predicted weights.
#' @param original_data Tibble. The original data containing masking information.
#' @param config List. A configuration list containing the minimum and maximum weight constraints.
#'
#' @return A tibble with adjusted weights after applying the masking and box constraints.
#' @importFrom dplyr mutate if_else
#' @import checkmate
#' @importFrom cli cli_alert_info cli_alert_warning
#'
#' @examples
#' \dontrun{
#' # Example data
#' weight_predictions <- tibble::tibble(
#'   stock_id = rep(1:3, each = 3),
#'   date = rep(seq.Date(Sys.Date(), by = "days", length.out = 3), times = 3),
#'   pred_weight = c(0.2, -0.5, 0.8,  # First stock, all three dates
#'               0.3, 0.0, 0.2,   # Second stock
#'               -0.6, 0.7, -0.4) # Third stock
#' )
#'
#' original_data <- tibble::tibble(
#'   stock_id = rep(1:3, each = 3),
#'   date = rep(seq.Date(Sys.Date(), by = "days", length.out = 3), times = 3),
#'   mask = c(1,1,1,1,1,0,1,1,1) # 3 stocks are masked on the last day
#' )
#'
#' # Define a model config with box constraints:
#' model_config <- list(
#'   min_weight = -0.5,
#'   max_weight = 0.5
#' )
#'}
#' @export
postprocess_weights <- function(weight_predictions, original_data, config) {
  # Check input types
  checkmate::assert_tibble(weight_predictions, min.rows = 1)
  checkmate::assert_tibble(original_data, min.rows = 1)
  checkmate::assert_list(config)

  # Extract constraints from config
  min_weight <- config$min_weight %||% -1
  max_weight <- config$max_weight %||% 1

  # Apply box constraints
  weight_predictions_m <- weight_predictions %>%
    dplyr::mutate(
      pred_weight = pmin(pmax(pred_weight, min_weight), max_weight)
    )

  # Apply mask
  weight_predictions_m <- weight_predictions_m %>%
    left_join(original_data |>  select(stock_id, date, mask), by = c("stock_id", "date")) %>%
    dplyr::mutate(
      pred_weight = if_else(mask == 1, pred_weight, 0)
    )

  return(weight_predictions_m)
}
