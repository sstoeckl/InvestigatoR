#' Create returnPrediction S3 object
#'
#' @param data Data that will be used for prediction
#' @param label Column name of the actual return
#'
#' @return returnPrediction S3 object
#'
#' @importFrom dplyr select
#' @importFrom tibble tibble
#'
#' @export
#'
#' @examples
#' # Initialize the returnPrediction object
#' data <- tibble::tibble(
#'   stock_id = 1:100,
#'   date = seq.Date(Sys.Date(), by = "day", length.out = 100),
#'   return_label = runif(100)
#' )
#' # Create the returnPrediction object
#' rp <- create_return_prediction(data, "return_label")
#' # Check the initial setup
#' print(rp)
#'
create_return_prediction <- function(data, label) {
  # Validate the input data contains necessary columns
  if (!("stock_id" %in% names(data)) || !("date" %in% names(data)) || !(label %in% names(data))) {
    stop("Data must contain 'stock_id', 'date', and the specified label column.")
  }

  # Prepare the tibbles
  predictions <- data %>%
    dplyr::select(stock_id, date)

  actual_returns <- data %>%
    dplyr::select(stock_id, date, actual_return=!!rlang::sym(label))  # Ensure actual returns are included

  errors <- data %>%
    dplyr::select(stock_id, date)

  # Bundle into a list with the class 'returnPrediction'
  structure(list(
    models = list(),
    predictions = predictions,
    actual_returns = actual_returns,
    errors = errors
  ), class = "returnPrediction")
}

#' Add model prediction to returnPrediction S3 object
#'
#' @param return_prediction Add model prediction to this object
#' @param model_function Name of the prediction (model) function
#' @param config Configuration for the model
#' @param new_predictions Tibble with stock_id, date, and prediction columns
#'
#' @return returnPrediction S3 object with added model prediction
#'
#' @importFrom dplyr mutate
#' @importFrom tibble tibble
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Initialize the returnPrediction object
#' data <- tibble::tibble(
#'   stock_id = 1:100,
#'   date = seq.Date(Sys.Date(), by = "day", length.out = 100),
#'   return_label = runif(100)
#' )
#' # Create the returnPrediction object
#' rp <- create_return_prediction(data, "return_label")
#' # Sample new prediction data
#' new_predictions <- tibble::tibble(
#'   stock_id = 1:100,
#'   date = seq.Date(Sys.Date(), by = "day", length.out = 100),
#'   prediction = runif(100)  # Example prediction values
#'   )
#' # Model function and config for demonstration
#' model_function <- "xgb_pred"  # Placeholder for your model function
#' config <- list(max_depth = 4, eta = 0.1)  # Example configuration
#' # Adding the new model predictions
#' rp <- add_model_prediction(rp, model_function, config, new_predictions)
#' print(rp$predictions)
#' print(rp$errors)
#' print(rp$models)
#' }
add_model_prediction <- function(return_prediction, model_function, config, new_predictions) {
  # Validate new_predictions includes necessary columns
  if (!("stock_id" %in% names(new_predictions)) || !("date" %in% names(new_predictions))) {
    stop("new_predictions must contain 'stock_id' and 'date' columns.")
  }

  # Generate a unique identifier for the model
  model_name <- sub("_pred$", "", model_function)
  existing_ids <- names(return_prediction$predictions)[-c(1,2)]  # Skip stock_id and date columns
  counter <- sum(grepl(paste0("^", model_name, "_\\d+$"), existing_ids)) + 1
  model_id <- paste0(model_name, "_", counter)

  # Add model details
  return_prediction$models[[model_id]] <- list(
    function_name = model_function,
    config = config,
    datetime = Sys.time(),
    identifier = model_id
  )

  # Calculate and join new errors
  new_errors <- new_predictions %>%
    dplyr::mutate(error=prediction - return_prediction$actual_returns$actual_return) |> dplyr::select(-prediction)

  # Prepare new_predictions by renaming the prediction column to the model identifier
  names(new_predictions)[names(new_predictions) == "prediction"] <- model_id
  names(new_errors)[names(new_errors) == "error"] <- model_id

  # Join new predictions with existing predictions
  return_prediction$predictions <- return_prediction$predictions %>%
    dplyr::left_join(new_predictions, by = c("stock_id", "date"))

  return_prediction$errors <- return_prediction$errors %>%
    dplyr::left_join(new_errors, by = c("stock_id", "date"))

  return(return_prediction)
}

# R/add_extra_data.R

#' Add Extra Data to returnPrediction S3 Object
#'
#' This function allows users to add supplementary datasets to an existing `returnPrediction` object. The extra data can include any relevant information, but must have the same `stock_id` and `date` combinations as the existing data in the object.
#'
#' @param return_prediction_object An object of class `returnPrediction`.
#' @param extra_data A tibble or data.frame containing additional data to be added. Must include `stock_id` and `date` columns and match the dimensions of the existing data.
#' @param new_data_name A character string specifying the name under which the extra data will be stored within the `returnPrediction` object.
#'
#' @return An updated `returnPrediction` object with the added extra data.
#'
#' @examples
#' \dontrun{
#' # Initialize the returnPrediction object
#' data <- tibble::tibble(
#'   stock_id = 1:100,
#'   date = seq.Date(Sys.Date(), by = "day", length.out = 100),
#'   return_label = runif(100)
#' )
#' # Create the returnPrediction object
#' rp <- create_return_prediction(data, "return_label")
#'
#' # Add extra data regarding weights.
#' extra_data <- tibble::tibble(
#'   stock_id = 1:100,
#'    date = seq.Date(Sys.Date(), by = "day", length.out = 100),
#'   sector = sample(c("Technology", "Healthcare", "Finance"), 100, replace = TRUE),
#'   country = sample(c("USA", "Germany", "Japan"), 100, replace = TRUE)
#' )
# Add extra data to returnPrediction object
#' rp <- add_extra_data(rp, extra_data, new_data_name = "sector_info")
#'
#' # View the added extra data
#' print(rp$extra_data$sector_info)
#' }
#'
#' @importFrom dplyr select left_join
#' @importFrom tibble tibble
#' @importFrom cli cli_alert_info cli_alert_success cli_alert_warning cli_alert_danger
#' @export
add_extra_data <- function(return_prediction_object, extra_data, new_data_name = "extra_data") {
  # Input validation
  if (!inherits(return_prediction_object, "returnPrediction")) {
    cli::cli_alert_danger("`return_prediction_object` must be of class 'returnPrediction'.")
    stop("`return_prediction_object` must be of class 'returnPrediction'.")
  }

  if (!is.data.frame(extra_data)) {
    cli::cli_alert_danger("`extra_data` must be a tibble or data.frame.")
    stop("`extra_data` must be a tibble or data.frame.")
  }

  required_cols <- c("stock_id", "date")
  missing_cols <- setdiff(required_cols, names(extra_data))
  if (length(missing_cols) > 0) {
    cli::cli_alert_danger("`extra_data` is missing required columns: {paste(missing_cols, collapse = ', ')}.")
    stop("`extra_data` must contain 'stock_id' and 'date' columns.")
  }

  # Check if the dimensions of extra_data match the dimensions of existing data
  if (!all(nrow(extra_data) == nrow(return_prediction_object$predictions))) {
    cli::cli_alert_danger("The dimensions of `extra_data` do not match the existing data in the returnPrediction object.")
    stop("Dimensions mismatch: Ensure `extra_data` has the same number of rows as the existing data in `returnPrediction`.")
  }

  # Check that the stock_id and date combinations are consistent
  key_cols <- c("stock_id", "date")
  existing_keys <- return_prediction_object$predictions %>%
    dplyr::select(dplyr::all_of(key_cols))

  new_keys <- extra_data %>%
    dplyr::select(dplyr::all_of(key_cols))

  if (!identical(existing_keys, new_keys)) {
    cli::cli_alert_danger("The `stock_id` and `date` combinations in `extra_data` do not match the existing data.")
    stop("Mismatch in `stock_id` and `date` combinations.")
  }

  # Initialize 'extra_data' list if it doesn't exist
  if (is.null(return_prediction_object$extra_data)) {
    return_prediction_object$extra_data <- list()
  }

  # If extra data already exists for the new_data_name, perform left join
  if (new_data_name %in% names(return_prediction_object$extra_data)) {
    cli::cli_alert_info("Existing extra data for '{new_data_name}' found. Performing a left join.")

    existing_data <- return_prediction_object$extra_data[[new_data_name]]

    # Perform a left join, handling column name conflicts
    overlapping_cols <- intersect(names(existing_data), names(extra_data))[-(1:2)]  # Exclude stock_id and date
    if (length(overlapping_cols) > 0) {
      cli::cli_alert_warning("Overwriting existing columns: {paste(overlapping_cols, collapse = ', ')} in the extra data.")
      extra_data <- extra_data %>% dplyr::select(-dplyr::all_of(overlapping_cols))  # Remove overlapping columns from new data
    }

    return_prediction_object$extra_data[[new_data_name]] <- dplyr::left_join(existing_data, extra_data, by = key_cols)

    cli::cli_alert_success("Successfully merged new data with existing extra data under '{new_data_name}'.")
  } else {
    # Add the new extra data
    return_prediction_object$extra_data[[new_data_name]] <- extra_data
    cli::cli_alert_success("Extra data '{new_data_name}' has been successfully added to the returnPrediction object.")
  }

  return(return_prediction_object)
}

# check.returnPrediction <- function(object,all=TRUE) {
#   required_fields <- c("models", "predictions", "actual_returns", "errors")
#
#   # Check if all required fields are present
#   missing_fields <- setdiff(required_fields, names(return_prediction))
#   if (length(missing_fields) > 0) {
#     stop("Missing required fields: ", paste(missing_fields, collapse=", "))
#   }
#
#   # Check each part of the return_prediction structure
#   if (!all(sapply(return_prediction$models, function(x) all(c("function_name", "config", "datetime", "identifier") %in% names(x))))) {
#     stop("One or more models are missing required information.")
#   }
#
#   # Check if predictions and actual_returns have consistent dimensions
#   if (nrow(return_prediction$predictions) != nrow(return_prediction$actual_returns)) {
#     stop("Predictions and actual returns do not match in number of rows.")
#   }
#
#   # Check if errors table aligns with predictions
#   if (!all(names(return_prediction$predictions)[-c(1, 2)] == names(return_prediction$errors)[-c(1, 2)])) {
#     stop("Mismatch in prediction and error identifiers.")
#   }
#
#   print("Return prediction object is consistent.")
# }

#' Calculate errors between predictions and actual returns
#'
#' @param predictions Predictions to be evaluated
#' @param actual_returns Actual returns
#'
#' @return List with Mean Squared Error (MSE), Root Mean Squared Error (RMSE), Mean Absolute Error (MAE), and Hit Ratio
#'
calculate_errors <- function(predictions, actual_returns) {
  mse <- mean((predictions - actual_returns)^2)
  rmse <- sqrt(mse)
  mae <- mean(abs(predictions - actual_returns))
  hit_ratio <- mean(sign(predictions) == sign(actual_returns))
  list(MSE = mse, RMSE = rmse, MAE = mae, Hit_Ratio = hit_ratio)
}
#' Summary method for returnPrediction S3 object
#'
#' @param object returnPrediction object
#' @param benchmark Actual returns to compare against (not yet implemented)
#'
#' @return Data frame with error statistics for each model
#' @export
#'
summary.returnPrediction <- function(return_prediction_object, benchmark = NULL) {
  # Calculate errors and regression statistics for each model
  results <- lapply(names(return_prediction_object$predictions)[-c(1,2)], function(model_id) {
    pred_col <- return_prediction_object$predictions[[model_id]]
    actual_col <- return_prediction_object$actual_returns$actual_return
    errors <- calculate_errors(pred_col, actual_col)
    })

  # Convert list of results to a data frame
  results_df <- do.call(rbind, results)
  rownames(results_df) <- names(return_prediction_object$predictions)[-c(1,2)]
  results_df
}

#' Create portfolioReturns S3 object
#'
#' Initializes a `portfolioReturns` S3 object with the specified structure.
#'
#' @param data A tibble or data frame containing portfolio and benchmark data. Must include `date` and `stock_id` columns.
#' @param return_label A string specifying the column name in `data` that contains the actual returns.
#' @param benchmark_label An optional string specifying the benchmark identifier. If provided, the function initializes benchmark-related slots.
#'
#' @return An S3 object of class `portfolioReturns` containing initialized slots based on the presence of a benchmark.
#' @importFrom dplyr select distinct
#' @importFrom tibble tibble
#' @importFrom cli cli_alert_success cli_alert_warning cli_alert_danger
#' @importFrom checkmate assert_data_frame assert_string assert_subset
#'
#' @examples
#' data <- tibble::tibble(
#'   stock_id = 1:100,
#'   date = rep(Sys.Date(), 100),
#'   return_label = runif(100)
#' )
#' pf <- create_portfolioReturns(data, "return_label")
#' print(pf)
#' @export
create_portfolioReturns <- function(data, return_label, benchmark_label = NULL) {
  # Load necessary libraries
  checkmate::assert_data_frame(data, any.missing = FALSE)
  checkmate::assert_string(return_label)

  # Validate input data
  checkmate::assert_subset(c("date", "stock_id"), choices = names(data))

  if (!(return_label %in% names(data))) {
    cli::cli_abort("Return label '{return_label}' not found in `data`.")
  }

  # Extract actual_returns
  actual_returns <- data %>%
    dplyr::select(stock_id, date, actual_return = !!rlang::sym(return_label)) %>%
    arrange(stock_id, date)

  # Create empty structures for portfolio results and models
  weights <- data %>% dplyr::select(stock_id, date)%>%
    arrange(stock_id, date)
  portfolio_returns <- data %>% dplyr::distinct(date)%>%
    arrange(date)

  # Initialize delta_weights and benchmark_returns if benchmark_label is provided
  if (!is.null(benchmark_label)) {
    # Validate benchmark_label in data
    if (!(benchmark_label %in% names(data))) {
      cli::cli_abort("Benchmark label '{benchmark_label}' not found in `data`.")
    }

    # Extract benchmark_returns
    delta_weights <- data %>% dplyr::select(stock_id, date) %>%  arrange(stock_id, date)
    benchmark_weights <- data %>% dplyr::select(stock_id, date, benchmark_weight = !!rlang::sym(benchmark_label)) %>%  arrange(stock_id, date)
    benchmark_returns <- data %>%
      dplyr::select(stock_id, date, actual_return = !!rlang::sym(return_label), benchmark_weight = !!rlang::sym(benchmark_label)) %>%
      dplyr::mutate(benchmark_return = actual_return * benchmark_weight) %>%
      group_by(date) |>
      summarise(benchmark_return = sum(benchmark_return), .groups = "drop") %>%
      arrange(date)

  } else {
    # Non-benchmark portfolio
    benchmark_returns <- NULL
    benchmark_weights <- NULL
    delta_weights <- NULL
  }

  # Create the portfolioReturns object
  portfolioReturns <- structure(
    list(
      models = list(),
      postprocessing_config = list(),
      weights = weights,
      delta_weights = delta_weights,
      benchmark_weights = benchmark_weights,
      actual_returns = actual_returns,
      portfolio_returns = portfolio_returns,
      benchmark_returns = benchmark_returns
    ),
    class = "portfolioReturns"
  )

  # Inform the user
  if (!is.null(benchmark_label)) {
    cli::cli_alert_success("Created portfolioReturns object with benchmark '{benchmark_label}'.")
  } else {
    cli::cli_alert_success("Created portfolioReturns object without a benchmark.")
  }

  return(portfolioReturns)
}

#' Add a Weight Model to a portfolioReturns Object
#'
#' Adds a new weight model to an existing `portfolioReturns` object. Handles both benchmark and non-benchmark portfolios by adjusting weights and updating portfolio returns accordingly.
#'
#' @param portfolio_object An existing `portfolioReturns` object.
#' @param model_name A string specifying the name of the new weight model.
#' @param new_weights A tibble or data frame containing the new weights with columns `date`, `stock_id`, and `weight`.
#'
#' @return An updated `portfolioReturns` object with the new weight model added.
#'
#' @importFrom dplyr select left_join inner_join mutate group_by summarise arrange
#' @importFrom cli cli_alert_success cli_alert_warning cli_alert_danger
#' @importFrom checkmate assert_class assert_string assert_list assert_data_frame assert_subset
#'
#' @examples
#' \dontrun{
#' # Adding a weight model to a non-benchmark portfolio
#' portfolio_obj <- add_weight_model(
#'   portfolio_object = portfolio_obj,
#'   model_name = "Model_A",
#'   new_weights = new_weights_df
#' )
#'
#' # Adding a weight model to a benchmark portfolio
#' portfolio_obj_bm <- add_weight_model(
#'   portfolio_object = portfolio_obj_bm,
#'   model_name = "Model_B",
#'   new_weights = new_weights_bm_df,
#'   benchmark_weights = benchmark_weights_df
#' )
#' }
#'
#' @export
add_weight_model <- function(portfolio_object, model_name, new_weights, config, postprocessing_config=NULL) {
  # Validate portfolio_object class
  checkmate::assert_class(portfolio_object, classes = "portfolioReturns")

  # Validate model_name
  checkmate::assert_string(model_name)

  # Validate new_weights
  checkmate::assert_data_frame(new_weights, any.missing = FALSE)
  checkmate::assert_subset(c("date", "stock_id", "pred_weight"), choices = names(new_weights))

  # Check if the portfolio has a benchmark
  has_benchmark <- !is.null(portfolio_object$benchmark_returns) && !is.null(portfolio_object$delta_weights)

  # Generate a unique identifier for the new weight model
  existing_ids <- names(portfolio_object$weights)[!(names(portfolio_object$weights) %in% c("stock_id", "date"))]
  model_id <- paste0(model_name, "_", length(existing_ids) + 1)

  # Add model details
  portfolio_object$models[[model_id]] <- list(
    model_name=model_name,
    config = config,
    datetime = Sys.time(),
    identifier = model_id
  )

  # Add postprocessing details
  if(!is.null(postprocessing_config)){
    portfolio_object$postprocessing_config[[model_id]] <- list(
      postprocessing_config = postprocessing_config,
      datetime = Sys.time(),
      identifier = model_id
    )
  }

  if (has_benchmark) {
    # Benchmark Portfolio Handling

    # Ensure new_weights and benchmark_weights have matching dates and stock_ids
    actual_dates <- unique(portfolio_object$actual_returns$date)
    actual_stock_ids <- unique(portfolio_object$actual_returns$stock_id)

    checkmate::assert_subset(new_weights$date, choices = actual_dates, empty.ok = FALSE)
    checkmate::assert_subset(new_weights$stock_id, choices = actual_stock_ids, empty.ok = FALSE)

    # Prepare new weights and join with existing weights
    names(new_weights)[3] <- model_id
    portfolio_object$delta_weights <- portfolio_object$delta_weights %>%
      dplyr::left_join(new_weights, by = c("stock_id", "date")) %>%
      arrange(stock_id, date)

    # Corrected typo from 'benachmark_weights' to 'benchmark_weights'
    benchmark_name <- names(portfolio_object$benchmark_weights)[3]  # Assuming the third column is 'benchmark_weight'

    # Calculate final_weights = benchmark_weights + delta_weights
    final_weights <- portfolio_object$benchmark_weights %>%
      dplyr::inner_join(new_weights, by = c("date", "stock_id")) %>%
      dplyr::mutate(!!model_id := .data[[benchmark_name]] + .data[[model_id]]) %>%
      dplyr::select(stock_id, date, !!model_id) %>%
      arrange(stock_id, date)

    # Prepare new weights and join with existing weights
    names(final_weights)[3] <- model_id
    portfolio_object$weights <- portfolio_object$weights %>%
      dplyr::left_join(final_weights, by = c("stock_id", "date")) %>%
      arrange(stock_id, date)

    # Calculate portfolio returns for the new model
    new_portfolio_returns <- final_weights %>%
      dplyr::left_join(portfolio_object$actual_returns, by = c("stock_id", "date")) %>%
      dplyr::group_by(date) %>%
      dplyr::summarise(portfolio_return = sum(actual_return * .data[[model_id]], na.rm = TRUE), .groups = 'drop') %>%
      dplyr::arrange(date)

    names(new_portfolio_returns)[2] <- model_id
    portfolio_object$portfolio_returns <- portfolio_object$portfolio_returns %>%
      dplyr::left_join(new_portfolio_returns, by = "date") %>%
      arrange(date)

    # Inform the user
    cli::cli_alert_success("Added weight model '{model_name}' to benchmark portfolio.")

  } else {
    # Non-Benchmark Portfolio Handling

    # Prepare new weights and join with existing weights
    names(new_weights)[3] <- model_id
    portfolio_object$weights <- portfolio_object$weights %>%
      dplyr::left_join(new_weights, by = c("stock_id", "date")) %>%
      arrange(stock_id, date)

    # Calculate portfolio returns for the new model
    new_portfolio_returns <- portfolio_object$weights %>%
      dplyr::left_join(portfolio_object$actual_returns, by = c("stock_id", "date")) %>%
      dplyr::group_by(date) %>%
      dplyr::summarise(portfolio_return = sum(actual_return * .data[[model_id]], na.rm = TRUE), .groups = 'drop') %>%
      dplyr::arrange(date)

    names(new_portfolio_returns)[2] <- model_id
    portfolio_object$portfolio_returns <- portfolio_object$portfolio_returns %>%
      dplyr::left_join(new_portfolio_returns, by = "date") %>%
      arrange(date)

    # Inform the user
    cli::cli_alert_success("Added weight model '{model_name}' to non-benchmark portfolio.")
  }

  # Return the updated portfolio_object
  return(portfolio_object)
}

# File: R/postprocessing_portfolios.R

#' Post-Process Portfolio Weights for Multiple Models
#'
#' Applies various constraints and adjustments to the portfolio weights based on a provided configuration.
#' Supports multiple models within a `portfolioReturns` object by processing each model independently.
#'
#' @param portfolio_object A `portfolioReturns` object.
#' @param config A list containing post-processing configuration parameters such as weight constraints, regularization, leverage limits, and smoothing windows.
#'
#' @return An updated `portfolioReturns` object with adjusted weights and recalculated portfolio returns for each model.
#'
#' @importFrom dplyr inner_join mutate select group_by summarise ungroup arrange
#' @importFrom zoo rollmean
#' @importFrom cli cli_alert_info cli_alert_warning cli_alert_success
#' @importFrom checkmate assert_class assert_list assert_names assert_string
#' @importFrom tidyr pivot_longer pivot_wider
#'
#' @examples
#' \dontrun{
#' # Define post-processing configuration
#' config <- list(
#'   list(
#'     operation = "set_weightsum",
#'     sum = 1,
#'     allow_short_sale = FALSE
#'   ),
#'   list(
#'     operation = "flatten_weights",
#'     l1 = 0.01,
#'     l2 = 0.01,
#'     mix = 0.7
#'   ),
#'   list(
#'     operation = "reduce_turnover",
#'     method = "exponential",
#'     smoothing_factor = 0.2
#'   ),
#'   list(
#'     operation = "increase_diversification",
#'     hh_target = 0.3
#'   )
#' )
#'
#' # Apply post-processing
#' portfolio_obj <- postprocessing_portfolios(portfolio_obj, config)
#' }
#'
#' @export
postprocessing_portfolios <- function(portfolio_object, config) {
  # Load necessary libraries

  # -----------------------------
  # Input Validation using checkmate
  # -----------------------------

  # Validate portfolio_object
  assert_class(portfolio_object, classes = "portfolioReturns")

  # Validate config
  assert_list(config, min.len = 1)

  # Each element in config should be a list with at least an 'operation' key
  for (i in seq_along(config)) {
    assert_list(config[[i]])
    assert_names(names(config[[i]]), must.include = "operation")
    assert_string(config[[i]]$operation)
  }

  # Determine if portfolio has benchmark
  has_benchmark <- !is.null(portfolio_object$benchmark_returns) && !is.null(portfolio_object$delta_weights)

  # Inform user about the type of portfolio
  if (has_benchmark) {
    cli::cli_alert_info("Post-processing applied to a benchmark portfolio.")
  } else {
    cli::cli_alert_info("Post-processing applied to a non-benchmark portfolio.")
  }

  # -----------------------------
  # Define Helper Functions for Operations
  # -----------------------------

  # 1. set_weightsum
  set_weightsum <- function(weights_df,
                            sum_val = 1,
                            allow_short_sale = TRUE,
                            min_sum = NULL,
                            max_sum = NULL,
                            min_weight = NULL,
                            max_weight = NULL,
                            epsilon = 1e-6) {

    # Handle short selling constraints
    if (!allow_short_sale) {
      weights_df <- weights_df %>%
        mutate(weight = ifelse(weight < 0, 0, weight))
      cli::cli_alert_info("Short selling disallowed: Negative weights set to zero.")
    }

    # Apply min and max weight constraints
    if (!is.null(min_weight) || !is.null(max_weight)) {
      weights_df <- weights_df %>%
        mutate(weight = pmin(pmax(weight, ifelse(is.null(min_weight), -Inf, min_weight)),
                             ifelse(is.null(max_weight), Inf, max_weight)))
      cli::cli_alert_info("Applied min and/or max weight constraints.")
    }

    # Normalize weights based on sum_val
    if (is.null(sum_val)) {
      cli::cli_alert_warning("No sum value provided. Weights not normalized. Potentially we apply min_sum/max_sum later!")
      # After normalization, check and enforce min_sum and max_sum if specified
      if (!is.null(min_sum) || !is.null(max_sum)) {
        weights_df <- weights_df %>%
          group_by(model_id, date) %>%
          mutate(current_sum = sum(weight, na.rm = TRUE)) %>%
          mutate(
            # Scale up to meet min_sum
            weight = if (!is.null(min_sum) && min(current_sum) < min_sum) {
              weight * (min_sum / current_sum)
            } else {
              weight
            },
            # Scale down to meet max_sum
            weight = if (!is.null(max_sum) && max(current_sum) > max_sum) {
              weight * (max_sum / current_sum)
            } else {
              weight
            }
          ) %>%
          ungroup() %>%
          select(-current_sum)

        if (!is.null(min_sum)) {
          cli::cli_alert_info("Scaled up weights to meet the minimum sum requirement of {min_sum} per model per date.")
        }
        if (!is.null(max_sum)) {
          cli::cli_alert_info("Scaled down weights to meet the maximum sum requirement of {max_sum} per model per date.")
        }
      }

    } else if (sum_val != 0) {
      # For sum_val ≠ 0, scale weights to sum_val per group
      # first we need to check that there are no zero columns
      # Calculate the total weight per group (model_id and date)
      weights_df <- weights_df %>%
        group_by(model_id, date) %>%
        mutate(total = sum(weight, na.rm = TRUE)) %>%
        ungroup()

      # Identify groups where total weight is zero
      zero_total_groups <- weights_df %>%
        filter(total == 0) %>%
        distinct(model_id, date)

      if (nrow(zero_total_groups) > 0) {
        # Add epsilon to each weight in zero total groups
        weights_df <- weights_df %>%
          left_join(zero_total_groups %>% mutate(flag=1), by = c("model_id", "date")) %>%
          mutate(weight = ifelse(!is.na(flag), weight+epsilon, weight)) %>%
          select(-flag) %>%
          group_by(model_id, date) %>%
          mutate(total = sum(weight, na.rm = TRUE)) %>%
          ungroup()
        cli::cli_alert_warning("Added epsilon to weights in groups with zero total weight.")
      }

      weights_df <- weights_df %>%
        mutate(weight = ifelse(total == 0, weight, weight / total * sum_val))
      # Remove temporary 'total' column
      weights_df <- weights_df %>%
        select(-total)
    } else {
      # For sum_val = 0, balance positive and negative weights
      # check whether both min_sum and max_sum are defined and != NULL and wehterh min_sum=-max_sum, if not, throw an error using cli
      if (is.null(min_sum) | is.null(max_sum)){
        cli::cli_alert_error("For sum_val = 0, min_sum and max_sum must be defined")
      }
      # check whether min_sum=-max_sum
      if (min_sum != -max_sum){
        cli::cli_alert_error("For sum_val = 0, min_sum must be equal to -max_sum")
      }
      # Separate positive and negative weights
      weights_df <- weights_df %>%
        group_by(model_id, date) %>%
        mutate(weight=weight-mean(weight)) |>
        mutate(
          # Calculate the initial positive and negative sums
          pos_sum = sum(weight[weight > 0], na.rm = TRUE),
          neg_sum = sum(weight[weight < 0], na.rm = TRUE),

          # Replace zero sums with epsilon to avoid division by zero
          pos_sum = ifelse(pos_sum == 0, epsilon, pos_sum),
          neg_sum = ifelse(neg_sum == 0, -epsilon, neg_sum),

          # Adjust negative sum to match the desired scaling constraints
          neg_sum_scaled = -(min(abs(min_sum),abs(max_sum))),
          pos_sum_scaled = -neg_sum_scaled,

          # Scale positive and negative weights independently
          weight = ifelse(
            weight > 0,
            weight / pos_sum * pos_sum_scaled,
            weight / abs(neg_sum) * abs(neg_sum_scaled)
          ),
          # Calculate the initial positive and negative sums
          pos_sum = sum(weight[weight > 0], na.rm = TRUE),
          neg_sum = sum(weight[weight < 0], na.rm = TRUE)
        ) %>%
        ungroup() %>%
        select(-pos_sum, -neg_sum, -neg_sum_scaled, -pos_sum_scaled)

    }

    # Final clamp to ensure weights are within min and max after scaling
    if (!is.null(min_weight) || !is.null(max_weight)) {
      weights_df <- weights_df %>%
        mutate(weight = pmin(pmax(weight, ifelse(is.null(min_weight), -Inf, min_weight)),
                             ifelse(is.null(max_weight), Inf, max_weight)))
      cli::cli_alert_info("Final clamp of weights to respect min and/or max constraints.")
    }

    return(weights_df)
  }

  # 2. flatten_weights
  flatten_weights <- function(weights_df, l1 = NULL, l2 = NULL, mix = 1) {

    # Apply Elastic Net-like mixing per model and date
    if (!is.null(mix) && mix > 0 && mix < 1) {
      if (!is.null(l1) && l1 > 0 && !is.null(l2) && l2 > 0) {
        weights_df <- weights_df %>%
          group_by(model_id, date) %>%
          mutate(weight = mix * (sign(weight) * pmax(abs(weight) - l1, 0)) +
                   (1 - mix) * (weight * (1 - l2))) %>%
          ungroup()
        cli::cli_alert_info("Applied mixed L1 and L2 regularization (Elastic Net) per model per date.")
      }
    } else {
      # Apply L1 regularization (soft thresholding) per model and date
      if (!is.null(l1) && l1 > 0) {
        weights_df <- weights_df %>%
          group_by(model_id, date) %>%
          mutate(weight = sign(weight) * pmax(abs(weight) - l1, 0)) %>%
          ungroup()
        cli::cli_alert_info("Applied L1 regularization: Weights shrunk towards zero per model per date.")
      }

      # Apply L2 regularization (shrinkage) per model and date
      if (!is.null(l2) && l2 > 0) {
        weights_df <- weights_df %>%
          mutate(weight = weight * (1 - l2))
        cli::cli_alert_info("Applied L2 regularization: Weights scaled down per model per date.")
      }
    }

    return(weights_df)
  }

  # 3. reduce_turnover
  reduce_turnover <- function(weights_df, method = "linear", smoothing_factor = 0.1) {
    # Arrange data by model_id, stock_id, and date
    weights_df <- weights_df %>%
      arrange(model_id, stock_id, date) %>%
      group_by(model_id, stock_id) %>%
      mutate(weight_prev = dplyr::lag(weight, default = weight[1])) %>%
      ungroup()

    # Apply smoothing based on method
    if (method == "linear") {
      weights_df <- weights_df %>%
        mutate(weight = weight_prev + (weight - weight_prev) * (1 - smoothing_factor))
      cli::cli_alert_info("Applied linear smoothing to reduce turnover per model per stock.")
    } else if (method == "exponential") {
      weights_df <- weights_df %>%
        mutate(weight = weight_prev * (1 - smoothing_factor) + weight * smoothing_factor)
      cli::cli_alert_info("Applied exponential smoothing to reduce turnover per model per stock.")
    } else {
      cli::cli_alert_warning("Unknown smoothing method '{method}'. Skipping turnover reduction.")
    }

    # Remove temporary column
    weights_df <- weights_df %>%
      select(-weight_prev)

    return(weights_df)
  }

  # 4. increase_diversification
  increase_diversification <- function(weights_df, hh_target = NULL) {
    # Herfindahl-Hirschman Index (HHI) target diversification
    if (is.null(hh_target)) {
      cli::cli_alert_warning("HHI target not provided. Skipping diversification enhancement.")
      return(weights_df)
    } else {
    # Calculate current HHI per model and date
    hhi_df <- weights_df %>%
      group_by(model_id, date) %>%
      summarize(hhi = sum(weight^2, na.rm = TRUE), .groups = 'drop')

    # Identify model-date combinations where HHI exceeds target
    models_to_adjust <- hhi_df %>%
      filter(hhi > hh_target) %>%
      select(model_id, date, hhi)

    if (nrow(models_to_adjust) == 0) {
      cli::cli_alert_info("Current diversification meets or exceeds all HHI targets.")
      return(weights_df)
    }

    # Adjust weights for model-date combinations exceeding HHI target
    weights_df <- weights_df %>%
      left_join(models_to_adjust %>%  mutate(flag=1), by = c("model_id", "date")) %>%
      mutate(
        adjustment_factor = ifelse(!is.na(flag), sqrt(hh_target / hhi), 1),
        weight = weight * adjustment_factor
      ) %>%
      select(-flag, -hhi, -adjustment_factor)

    cli::cli_alert_info("Enhanced diversification by adjusting weights to meet HHI targets per model per date.")
    }
    return(weights_df)
  }

  # -----------------------------
  # Reshape Weights to Long Format for Multiple Models
  # -----------------------------

  # Identify all model_ids from portfolio_object$models
  model_ids <- names(portfolio_object$models)
  if (length(model_ids) == 0) {
    cli::cli_abort("No models found in `portfolio_object$models`.")
  }

  # If benchmark exists, reshape delta_weights similarly
  if (has_benchmark) {
    # Reshape delta_weights to long format
    weights_long <- portfolio_object$delta_weights %>%
      pivot_longer(
        cols = all_of(model_ids),
        names_to = "model_id",
        values_to = "weight"
      ) %>% na.omit()
    } else {
    # Join weights_long with delta_weights_long
    weights_long <- portfolio_object$weights %>%
      pivot_longer(
        cols = all_of(model_ids),
        names_to = "model_id",
        values_to = "weight"
      ) %>% na.omit()
  }

  # -----------------------------
  # Apply Post-Processing Operations to Each Model
  # -----------------------------

  # Apply each operation in the config sequentially
  for (i in seq_along(config)) {
    op <- config[[i]]
    operation <- op$operation
    cli::cli_alert_info("Applying operation {i}: {operation}")

    if (operation == "set_weightsum") {
      # Extract parameters with defaults
      sum_val <- op$sum %||% NULL
      allow_short_sale <- op$allow_short_sale %||% TRUE
      min_sum <- op$min_sum %||% NULL
      max_sum <- op$max_sum %||% NULL
      min_weight <- op$min_weight %||% NULL
      max_weight <- op$max_weight %||% NULL

      weights_long <- set_weightsum(
        weights_df = weights_long,
        sum_val = sum_val,
        allow_short_sale = allow_short_sale,
        min_sum = min_sum,
        max_sum = max_sum,
        min_weight = min_weight,
        max_weight = max_weight
      )

    } else if (operation == "set_weights") {
      # Extract parameters
      min_weight <- if (!is.null(op$min_weight)) op$min_weight else NULL
      max_weight <- if (!is.null(op$max_weight)) op$max_weight else NULL

      # Clamp individual weights per model and date
      if (!is.null(min_weight) || !is.null(max_weight)) {
        weights_long <- weights_long %>%
          mutate(weight = pmin(pmax(weight, ifelse(is.null(min_weight), -Inf, min_weight)),
                               ifelse(is.null(max_weight), Inf, max_weight)))
        cli::cli_alert_info("Individual weights have been clamped to specified min and/or max values per model per date.")
      }

    } else if (operation == "flatten_weights") {
      # Extract parameters
      l1 <- op$l1 %||% NULL
      l2 <- op$l2 %||% NULL
      mix <- op$mix %||% 1

      weights_long <- flatten_weights(
        weights_df = weights_long,
        l1 = l1,
        l2 = l2,
        mix = mix
      )

    } else if (operation == "reduce_turnover") {
      # Extract parameters
      method <- op$method %||% "linear"
      smoothing_factor <- op$smoothing_factor %||% 0.1

      weights_long <- reduce_turnover(
        weights_df = weights_long,
        method = method,
        smoothing_factor = smoothing_factor
      )

    } else if (operation == "increase_diversification") {
      # Extract parameters
      hh_target <- op$hh_target %||% NULL

      weights_long <- increase_diversification(
        weights_df = weights_long,
        hh_target = hh_target
      )

    } else {
      cli::cli_alert_warning("Unknown operation '{operation}'. Skipping.")
    }
  }

  # -----------------------------
  # Reshape Processed Weights Back to Wide Format
  # -----------------------------

  # Pivot weights_long_processed back to wide format
  weights_wide_processed <- weights_long %>%
    tidyr::pivot_wider(
      names_from = model_id,
      values_from = weight,
      values_fill = 0
    )

  model_names <- names(weights_wide_processed)[-c(1,2)]

  if (has_benchmark){
  # -----------------------------
  # Update Delta Weights if Benchmark Exists
  # -----------------------------
    portfolio_object$delta_weights <- portfolio_object$delta_weights %>% select(1,2)
    portfolio_object$weights <- portfolio_object$weights %>% select(1,2)
    portfolio_object$portfolio_returns <- portfolio_object$portfolio_returns %>% select(1)
    pf_configs <- portfolio_object$models
    portfolio_object$models <- NULL
    pppf_configs <- portfolio_object$postprocessing_config
    portfolio_object$postprocessing_config <- NULL

    for (nn in model_names){
      # remove everything after "_" from name using gsub
      nn2 <- gsub("_[0-9]","",nn)

      portfolio_object <- add_weight_model(portfolio_object, nn2, weights_wide_processed %>% select(1,2,pred_weight=all_of(nn)),
                                           config=portfolio_object$models[[nn]],
                                           # bind older postprocessing_config with new one
                                           postprocessing_config=c(portfolio_object$postprocessing_config[[nn]],
                                                                   config)
                                           )
    }

  } else {
    # -----------------------------
    # Update Weights if no Benchmark Exists
    # -----------------------------
    lnames <- names(portfolio_object)
    portfolio_object$weights <- portfolio_object$weights %>% select(1,2)
    portfolio_object$portfolio_returns <- portfolio_object$portfolio_returns %>% select(1)
    pf_configs <- portfolio_object$models
    portfolio_object$models <- list()
    pppf_configs <- portfolio_object$postprocessing_config
    portfolio_object$postprocessing_config <- list()

    for (nn in model_names){
      # remove everything after "_" from name using gsub
      nn2 <- gsub("_[0-9]","",nn)

      portfolio_object <- add_weight_model(portfolio_object, nn2, weights_wide_processed %>% select(1,2,pred_weight=all_of(nn)),
                                           config=pf_configs[[nn]]$config,
                                           # bind older postprocessing_config with new one
                                           postprocessing_config=c(pppf_configs[[nn]],
                                                                   config)
      )
    }

  }

  # -----------------------------
  # Finalize Postprocessing Weights
  # -----------------------------

  # Recalculate portfolio_returns for each model (already done above)

  # Inform the user
  cli::cli_alert_success("Post-processing completed for all models.")

  return(portfolio_object)
}

#' Define the summary method for 'portfolioReturns' class
#'
#' @param portfolio_object A portfolioReturns object
#' @param type default=NULL (standard evaluation), Alternative: tq_performance_functions
#'
#' @return A summary satistics of the portfolio returns
#'
#' @import PerformanceAnalytics
#' @importFrom dplyr group_by
#' @importFrom tidyr pivot_longer
#'
#' @export
summary.portfolioReturns <- function(portfolio_object, type=NULL) {
    returns_data <- portfolio_object$portfolio_returns
    weights_data <- portfolio_object$weights
    actual_data <- portfolio_object$actual_returns
    # long_format
  returns_data_long <- returns_data %>%
    tidyr::pivot_longer(cols = -date, names_to = "portfolio", values_to = "returns")
  # now we do summary statistics
  if (is.null(type)){
    stats <- perf_met(returns_data, weights_data, actual_data)
  } else {
    # User specifies a PerformanceAnalytics plotting function
    xts_data <- timetk::tk_xts(returns_data)
    # Call the user-specified PerformanceAnalytics function
    stat_func <- match.fun(type)
    stats<-stat_func(xts_data)
  }

  return(stats)
}

#' Summarize Portfolio Performance with Advanced Metrics and Statistical Tests
#'
#' Provides a detailed summary of portfolio performance metrics,
#' including statistical tests for significance.
#'
#' @param portfolio_object A `portfolioReturns` object.
#' @param benchmark_data (Optional) A data frame with columns `date` and `return` representing an external benchmark.
#' @param test Logical. If `TRUE`, performs statistical tests for significance.
#' @param print Logical. If `TRUE`, prints the summary to the console.
#'
#' @return A data frame summarizing portfolio performance metrics and test results.
#'
#' @import PerformanceAnalytics dplyr timetk
#'
#' @export
summary.performance <- function(portfolio_object, benchmark_data = NULL, test = FALSE, print = FALSE) {

  # Validate input
  checkmate::assert_class(portfolio_object, classes = "portfolioReturns")
  if (!is.null(benchmark_data)) {
    checkmate::assert_data_frame(benchmark_data, any.missing = FALSE)
    checkmate::assert_subset(c("date", "return"), choices = names(benchmark_data))
  }

  # Extract portfolio returns (wide format)
  portfolio_returns <- portfolio_object$portfolio_returns

  # Handle benchmark: use external or equally weighted as default
  if (!is.null(benchmark_data)) {
    benchmark <- benchmark_data
    # add benchmark to portfolio_returns
    portfolio_returns <- portfolio_returns %>%
      left_join(benchmark, by = "date")
  } else {
    # if no benchmark is given, check whether portfolio object comes with own benchmark
    if (!is.null(portfolio_object$benchmark_returns)){
      benchmark <- portfolio_object$benchmark_returns %>% select(date, benchmark=benchmark_return)
      # add benchmark to portfolio_returns
      portfolio_returns <- portfolio_returns %>%
        left_join(benchmark, by = "date")
    } else {
    # Compute equally weighted portfolio as benchmark
    ew_returns <- portfolio_object$actual_returns %>%
      group_by(date) %>%
      summarise(benchmark = mean(actual_return, na.rm = TRUE), .groups = 'drop')
    benchmark <- ew_returns
    }
  }

  # Convert to xts format for PerformanceAnalytics
  portfolio_xts <- portfolio_returns %>%
    timetk::tk_xts()
  benchmark_xts <- benchmark %>%
    timetk::tk_xts(select = benchmark, date_var = date)

  # Initialize results data frame
  results <- tibble()

  # Loop over each portfolio column
  portfolio_names <- setdiff(names(portfolio_returns), "date")
  for (portfolio in portfolio_names) {
    # Extract portfolio and benchmark returns
    portfolio_data <- portfolio_xts[, portfolio, drop = FALSE]

    # Performance Metrics (Annualized where applicable)
    mean_return <- PerformanceAnalytics::table.AnnualizedReturns(portfolio_data,geometric=FALSE)[1, 1]
    volatility <- PerformanceAnalytics::table.AnnualizedReturns(portfolio_data,geometric=FALSE)[2, 1]
    sharpe_ratio <- PerformanceAnalytics::table.AnnualizedReturns(portfolio_data,geometric=FALSE)[3, 1]
    sortino_ratio <- PerformanceAnalytics::SortinoRatio(portfolio_data)
    max_drawdown <- PerformanceAnalytics::maxDrawdown(portfolio_data)
    skewness <- PerformanceAnalytics::skewness(portfolio_data)
    kurtosis <- PerformanceAnalytics::kurtosis(portfolio_data,method="excess")
    value_at_risk <- PerformanceAnalytics::VaR(portfolio_data, p = 0.05)
    conditional_var <- PerformanceAnalytics::ES(portfolio_data, p = 0.05)

    # Metrics relative to benchmark
    if (!is.null(benchmark_xts)) {
      alpha <- PerformanceAnalytics::CAPM.alpha(portfolio_data, benchmark_xts)
      beta <- PerformanceAnalytics::CAPM.beta(portfolio_data, benchmark_xts)
      tracking_error <- PerformanceAnalytics::TrackingError(portfolio_data, benchmark_xts)
      information_ratio <- PerformanceAnalytics::InformationRatio(portfolio_data, benchmark_xts)
    } else {
      alpha <- beta <- tracking_error <- information_ratio <- r_squared <- NA
    }

    # Statistical Tests (if requested)
    if (test) {
      # Mean return t-test
      mean_t_test_p <- t.test(as.numeric(portfolio_data), mu = 0)$p.value

      # Alpha significance
      alpha_t_test_p <- if (!is.null(benchmark_xts)) {
        t.test(as.numeric(portfolio_data - benchmark_xts))$p.value
      } else NA

      # Beta significance
      beta_t_test_p <- if (!is.null(benchmark_xts)) {
        lm_result <- lm(as.numeric(portfolio_data) ~ as.numeric(benchmark_xts))
        summary(lm_result)$coefficients[2, 4]
      } else NA

      # Information Ratio significance
      information_ratio_t_test_p <- if (!is.null(benchmark_xts)) {
        n <- nrow(portfolio_data)
        info_ratio <- as.numeric(information_ratio)
        t_stat <- sqrt(n) * info_ratio / sqrt(1 + info_ratio^2)
        2 * (1 - pt(abs(t_stat), df = n - 1))
      } else NA

      # Sharpe ratio Memmel significance
      sharpe_ratio_test_memmel_p <- MemmelSharpeTest(portfolio_data, benchmark_xts)
    } else {
      mean_t_test_p <- alpha_t_test_p <- beta_t_test_p <- information_ratio_t_test_p <- sharpe_ratio_test_memmel_p <- NA
    }

    # Combine results
    results <- rbind(
      results,
      tibble(
        Portfolio = portfolio,
        `Annualized Mean` = round(mean_return, 4),
        `Annualized Volatility` = round(volatility, 4),
        `Sharpe Ratio` = round(sharpe_ratio, 4),
        `Sortino Ratio` = round(sortino_ratio, 4),
        `Max Drawdown` = round(max_drawdown, 4),
        Skewness = round(skewness, 4),
        Kurtosis = round(kurtosis, 4),
        `Value at Risk` = round(value_at_risk, 4),
        `Conditional Value at Risk` = round(conditional_var, 4),
        Alpha = round(alpha, 4),
        Beta = round(beta, 4),
        `Tracking Error` = round(tracking_error, 4),
        `Information Ratio` = round(information_ratio, 4),
        `Mean T-Test P-Value` = round(mean_t_test_p, 4),
        `Alpha T-Test P-Value` = round(alpha_t_test_p, 4),
        `Beta T-Test P-Value` = round(beta_t_test_p, 4),
        `Information Ratio T-Test P-Value` = round(information_ratio_t_test_p, 4),
        `Sharpe Ratio Test (Memmel) P-Value` = round(sharpe_ratio_test_memmel_p$p, 4)
      )
    )
  }

  # Print table if `print` is TRUE
  if (print) {
    results_to_print <- results %>%
      mutate(
        `Annualized Mean` = ifelse(
          !is.na(`Mean T-Test P-Value`),
          paste0(round(`Annualized Mean`, 4),
                 case_when(
                   `Mean T-Test P-Value` <= 0.01 ~ "***",
                   `Mean T-Test P-Value` <= 0.05 ~ "**",
                   `Mean T-Test P-Value` <= 0.10 ~ "*",
                   TRUE ~ ""
                 )),
          round(`Annualized Mean`, 4)
        ),
        `Sharpe Ratio` = ifelse(
          !is.na(`Sharpe Ratio Test (Memmel) P-Value`),
          paste0(round(`Sharpe Ratio`, 4),
                 case_when(
                   `Sharpe Ratio Test (Memmel) P-Value` <= 0.01 ~ "***",
                   `Sharpe Ratio Test (Memmel) P-Value` <= 0.05 ~ "**",
                   `Sharpe Ratio Test (Memmel) P-Value` <= 0.10 ~ "*",
                   TRUE ~ ""
                 )),
          round(`Sharpe Ratio`, 4)
        ),
        `Information Ratio` = ifelse(
          !is.na(`Information Ratio T-Test P-Value`),
          paste0(round(`Information Ratio`, 4),
                 case_when(
                   `Information Ratio T-Test P-Value` <= 0.01 ~ "***",
                   `Information Ratio T-Test P-Value` <= 0.05 ~ "**",
                   `Information Ratio T-Test P-Value` <= 0.10 ~ "*",
                   TRUE ~ ""
                 )),
          round(`Information Ratio`, 4)
        ),
        `Alpha` = ifelse(
          !is.na(`Alpha T-Test P-Value`),
          paste0(round(`Alpha`, 4),
                 case_when(
                   `Alpha T-Test P-Value` <= 0.01 ~ "***",
                   `Alpha T-Test P-Value` <= 0.05 ~ "**",
                   `Alpha T-Test P-Value` <= 0.10 ~ "*",
                   TRUE ~ ""
                 )),
          round(`Alpha`, 4)
        ),
        `Beta` = ifelse(
          !is.na(`Beta T-Test P-Value`),
          paste0(round(`Beta`, 4),
                 case_when(
                   `Beta T-Test P-Value` <= 0.01 ~ "***",
                   `Beta T-Test P-Value` <= 0.05 ~ "**",
                   `Beta T-Test P-Value` <= 0.10 ~ "*",
                   TRUE ~ ""
                 )),
          round(`Beta`, 4)
        )
      ) %>%
      select(-`Mean T-Test P-Value`, -`Alpha T-Test P-Value`, -`Beta T-Test P-Value`, -`Information Ratio T-Test P-Value`, -`Sharpe Ratio Test (Memmel) P-Value`)

    print(knitr::kable(
      results_to_print,
      digits = 4,
      align = "c",
      format = "markdown",
      caption = "Portfolio Performance Summary"
    ))
  }

  return(results)
}

#' Summarize Advanced Portfolio Performance Metrics and Statistical Tests
#'
#' Provides a detailed summary of advanced portfolio performance metrics,
#' including statistical tests and transaction cost adjustments.
#'
#' @param portfolio_object A `portfolioReturns` object.
#' @param transaction_cost Numeric. Assumed transaction costs for turnover adjustment.
#' @param gamma Numeric. Risk aversion parameter for Certainty Equivalent Return.
#' @param test Logical. If `TRUE`, performs statistical tests for significance.
#' @param print Logical. If `TRUE`, prints a formatted table of results.
#'
#' @return A data frame summarizing portfolio performance metrics and test results.
#'
#' @import PerformanceAnalytics tidyquant dplyr
#'
#' @export
summary.performance2 <- function(portfolio_object, transaction_cost = 0.001, gamma = 3, test = FALSE, print = FALSE) {
  # Extract returns and weights
  portfolio_returns <- portfolio_object$portfolio_returns
  actual_returns <- portfolio_object$actual_returns
  weights <- portfolio_object$weights
  benchmark_weights <- portfolio_object$benchmark_weights
  benchmark_returns <- portfolio_object$benchmark_returns

  # Check if benchmark exists, otherwise create equal-weighted benchmark
  if (is.null(benchmark_weights) || is.null(benchmark_returns)) {
    unique_dates <- unique(portfolio_returns$date)
    benchmark_weights <- actual_returns %>%
      group_by(date) %>%
      mutate(benchmark_weight = 1 / n()) %>%
      ungroup() |>  select(-actual_return)
    benchmark_returns <- actual_returns %>%
      left_join(benchmark_weights, by = c("date", "stock_id")) %>%
      group_by(date) %>%
      summarize(benchmark_return = sum(actual_return * benchmark_weight, na.rm = TRUE), .groups = "drop")
  }

  # add benchmark returns to portfolio returns only if bm object or expl. given
  #if (!is.null(portfolio_object$benchmark_returns) && !is.null(portfolio_object$benchmark_weights)){
    portfolio_returns <- portfolio_returns %>%
      left_join(benchmark_returns|> rename(benchmark=benchmark_return), by = "date")
    # and to weights
    weights <- weights %>%
      left_join(benchmark_weights |> rename(benchmark=benchmark_weight), by = c("stock_id","date"))
  #}


  # Convert data to xts for PerformanceAnalytics compatibility
  portfolio_xts <- timetk::tk_xts(portfolio_returns, silent = TRUE)
  benchmark_xts <- timetk::tk_xts(benchmark_returns, silent = TRUE)
  weights_xts <- timetk::tk_xts(weights, silent = TRUE)
  benchmark_weights_xts <- timetk::tk_xts(benchmark_weights, silent = TRUE)

  # Initialize results data frame
  results <- tibble()

  # Loop through each portfolio
  for (portfolio_name in names(portfolio_xts)) {
    # Extract portfolio and benchmark returns
    portfolio_ret <- portfolio_xts[, portfolio_name]
    benchmark_ret <- benchmark_xts[, 1] # Assumes single benchmark column
    weights_ext <- weights_xts[, portfolio_name]
    benchmark_weights_ext <- benchmark_weights_xts[, 1] # Assumes single benchmark column

    # Annualized mean return
    annualized_mean <- PerformanceAnalytics::table.AnnualizedReturns(portfolio_ret,geometric=FALSE)[1,1]
    mean_pval <- if (test) {
      t.test(coredata(portfolio_ret), mu = mean(coredata(benchmark_ret)), alternative = "greater")$p.value
    } else {
      NA
    }

    # Sharpe ratio and Memmel-corrected test
    sharpe_ratio <- PerformanceAnalytics::table.AnnualizedReturns(portfolio_ret,geometric=FALSE)[3,1]
    memmel_test <- if (test) {
      memmel_result <- MemmelSharpeTest(coredata(portfolio_ret), coredata(benchmark_ret), alternative = "greater")
      list(statistic = memmel_result$f, p_value = memmel_result$p)
    } else {
      list(statistic = NA, p_value = NA)
    }

    # Active share
    active_share <- calculate_active_share(weights |> select(stock_id, date, weight=!!portfolio_name), benchmark_weights)

    # Turnover
    turnover_data <- calculate_turnover(weights |> select(stock_id, date, weight=!!portfolio_name), actual_returns)
    per_period_turnover <- turnover_data$per_period_turnover
    total_turnover <- turnover_data$total_turnover

    # Transaction cost-adjusted returns
    tc_adjusted_returns <- portfolio_ret - (per_period_turnover * transaction_cost)
    tc_sharpe_ratio <- PerformanceAnalytics::table.AnnualizedReturns(tc_adjusted_returns,geometric=FALSE)[3,1]
    tc_memmel_test <- if (test) {
      memmel_result <- MemmelSharpeTest(coredata(tc_adjusted_returns), coredata(benchmark_ret), alternative = "greater")
      list(statistic = memmel_result$f, p_value = memmel_result$p)
    } else {
      list(statistic = NA, p_value = NA)
    }

    # Information ratio
    tracking_error <- sd(portfolio_ret - benchmark_ret, na.rm = TRUE) * sqrt(12)
    information_ratio <- mean(portfolio_ret - benchmark_ret, na.rm = TRUE) / tracking_error
    ir_pval <- if (test) {
      t.test(coredata(portfolio_ret - benchmark_ret), alternative = "greater")$p.value
    } else {
      NA
    }

    # Certainty equivalent return
    cert_eq_return <- annualized_mean - (gamma / 2) * (sd(portfolio_ret, na.rm = TRUE) * sqrt(12))^2

    # Collect results for this portfolio
    # Combine results
    # Collect results for this portfolio
    results <- rbind(
      results,
      tibble(
        Portfolio = portfolio_name,
        `Annualized Mean` = round(annualized_mean, 4),
        `Mean P-Value` = round(mean_pval, 4),
        `Sharpe Ratio` = round(sharpe_ratio, 4),
        `Sharpe Test Stat.` = round(memmel_test$statistic, 4),
        `Sharpe Test P-Value` = round(memmel_test$p_value, 4),
        `Active Share` = round(active_share, 4),
        `Turnover` = round(total_turnover, 4),
        `TC Adjusted Sharpe` = round(tc_sharpe_ratio, 4),
        `TC Sharpe Test Stat.` = round(tc_memmel_test$statistic, 4),
        `TC Sharpe Test P-Value` = round(tc_memmel_test$p_value, 4),
        `Information Ratio` = round(information_ratio, 4),
        `IR P-Value` = round(ir_pval, 4),
        CER = round(cert_eq_return, 4)
      )
    )
  }

  # Add significance stars
  if (print) {
    results_to_print <- results %>%
      mutate(
        `Annualized Mean` = ifelse(
          !is.na(`Mean P-Value`),
          paste0(round(`Annualized Mean`, 4),
                 case_when(
                   `Mean P-Value` <= 0.01 ~ "***",
                   `Mean P-Value` <= 0.05 ~ "**",
                   `Mean P-Value` <= 0.10 ~ "*",
                   TRUE ~ ""
                 )),
          round(`Annualized Mean`, 4)
        ),
        `Sharpe Ratio` = ifelse(
          !is.na(`Sharpe Test P-Value`),
          paste0(round(`Sharpe Ratio`, 4),
                 case_when(
                   `Sharpe Test P-Value` <= 0.01 ~ "***",
                   `Sharpe Test P-Value` <= 0.05 ~ "**",
                   `Sharpe Test P-Value` <= 0.10 ~ "*",
                   TRUE ~ ""
                 )),
          round(`Sharpe Ratio`, 4)
        ),
        `TC Adjusted Sharpe` = ifelse(
          !is.na(`TC Sharpe Test P-Value`),
          paste0(round(`TC Adjusted Sharpe`, 4),
                 case_when(
                   `TC Sharpe Test P-Value` <= 0.01 ~ "***",
                   `TC Sharpe Test P-Value` <= 0.05 ~ "**",
                   `TC Sharpe Test P-Value` <= 0.10 ~ "*",
                   TRUE ~ ""
                 )),
          round(`TC Adjusted Sharpe`, 4)
        ),
        `Information Ratio` = ifelse(
          !is.na(`IR P-Value`),
          paste0(round(`Information Ratio`, 4),
                 case_when(
                   `IR P-Value` <= 0.01 ~ "***",
                   `IR P-Value` <= 0.05 ~ "**",
                   `IR P-Value` <= 0.10 ~ "*",
                   TRUE ~ ""
                 )),
          round(`Information Ratio`, 4)
        )
      ) %>%
      rename(!!paste0("CER (gamma=",gamma,")") := CER) |>
      select(-`Mean P-Value`, -`Sharpe Test P-Value`, -`TC Sharpe Test P-Value`, -`IR P-Value`, -`Sharpe Test Stat.`, -`TC Sharpe Test Stat.`)

    print(knitr::kable(
      results_to_print,
      digits = 4,
      align = "c",
      format = "markdown",
      caption = "Portfolio Performance Summary"
    ))
  }

  return(results)
}

#' Summarize Portfolio Correlations
#'
#' Computes and optionally prints a correlation table between portfolio returns,
#' with the benchmark being the first column.
#'
#' @param portfolio_object A `portfolioReturns` object.
#' @param benchmark_data (Optional) A data frame with columns `date` and `return` representing an external benchmark.
#' @param print Logical. If `TRUE`, prints the correlation table using `kable`.
#' @return A correlation matrix between portfolio returns and the benchmark.
#'
#' @import knitr dplyr tidyr
#' @export
summary.correlation <- function(portfolio_object, benchmark_data = NULL, print = FALSE) {
  # Validate input
  checkmate::assert_class(portfolio_object, classes = "portfolioReturns")
  if (!is.null(benchmark_data)) {
    checkmate::assert_data_frame(benchmark_data, any.missing = FALSE)
    checkmate::assert_subset(c("date", "return"), choices = names(benchmark_data))
  }

  # Extract portfolio returns (wide format)
  portfolio_returns <- portfolio_object$portfolio_returns

  # Handle benchmark: use external or equally weighted as default
  if (!is.null(benchmark_data)) {
    benchmark <- benchmark_data %>%
      rename(benchmark = return)
  } else {
    # Compute equally weighted portfolio as benchmark
    benchmark <- portfolio_object$actual_returns %>%
      group_by(date) %>%
      summarise(benchmark = mean(actual_return, na.rm = TRUE), .groups = 'drop')
  }

  # Merge benchmark with portfolio returns
  combined_data <- portfolio_returns %>%
    left_join(benchmark, by = "date") %>%
    select(-date)

  # Compute the correlation matrix
  correlation_matrix <- cor(combined_data, use = "pairwise.complete.obs")

  # Optionally print the correlation table
  if (print) {
    # Format the matrix for display
    formatted_matrix <- as.data.frame(correlation_matrix)
    colnames(formatted_matrix) <- colnames(combined_data)
    rownames(formatted_matrix) <- colnames(combined_data)

    # Convert to a neat table using kable
    print(knitr::kable(
      formatted_matrix,
      format = "markdown",
      digits = 4,
      align = "c",
      caption = "Correlation Matrix of Portfolio Returns"
    ))
  }

  return(correlation_matrix)
}

#' Summarize Portfolio Weights with Advanced Options
#'
#' Computes summary statistics for portfolio weights, including deviations, turnover, L1/L2 norms, and weight stability.
#'
#' @param portfolio_object A `portfolioReturns` object.
#' @param use_delta Logical. If `TRUE` and the portfolio object has a benchmark, computes statistics for delta_weights. Defaults to `FALSE`.
#' @param print Logical. If `TRUE`, prints the summary in a nicely formatted table. Defaults to `FALSE`.
#'
#' @return A tibble with advanced portfolio weight statistics.
#'
#' @export
summary.weights <- function(portfolio_object, use_delta = FALSE, print = FALSE) {
  # Validate the portfolio object
  checkmate::assert_class(portfolio_object, classes = "portfolioReturns")

  # Determine weights to use
  if (use_delta && !is.null(portfolio_object$delta_weights)) {
    weights <- portfolio_object$delta_weights
  } else {
    weights <- portfolio_object$weights
  }

  # Check that weights are present
  if (is.null(weights) || ncol(weights) <= 2) {
    stop("No portfolio weights available for summary.")
  }

  # Extract the list of portfolios
  portfolio_names <- setdiff(names(weights), c("date", "stock_id"))

  # Compute weight statistics
  weight_summary <- weights %>%
    tidyr::pivot_longer(cols = -c(stock_id, date), names_to = "Portfolio", values_to = "Weight") %>%
    group_by(Portfolio, date) %>%
    mutate(
      `Date Std. Dev.` = sd(Weight, na.rm = TRUE),
      `HHI per Date` = sum(Weight^2, na.rm = TRUE), # Calculate HHI per date
      `Diversity per Date` = 1 / sum(Weight^2, na.rm = TRUE), # Diversity measure per date
      `L1 Norm per Date` = sum(abs(Weight), na.rm = TRUE), # L1 norm per date
      `L2 Norm per Date` = sqrt(sum(Weight^2, na.rm = TRUE)), # L2 norm per date
      `Short Weights per Date` = sum(Weight < 0, na.rm = TRUE), # Short weights per date
      .groups = "drop"
    ) %>%
    group_by(Portfolio) %>%
    summarise(
      `Weights (Min/Max)` = paste0(round(min(Weight, na.rm = TRUE), 4), "/", round(max(Weight, na.rm = TRUE), 4)),
      `Weights M(SD)` = paste0(
        round(mean(Weight, na.rm = TRUE), 4),
        " (", round(sd(Weight, na.rm = TRUE), 4), ")"
      ), # Mean with SD in brackets
      `Std.Dev./Date M(SD)` = paste0(
        round(mean(`Date Std. Dev.`, na.rm = TRUE), 4),
        " (", round(sd(`Date Std. Dev.`, na.rm = TRUE), 4), ")"
      ),
      `HHI M(SD)` = paste0(
        round(mean(`HHI per Date`, na.rm = TRUE), 4),
        " (", round(sd(`HHI per Date`, na.rm = TRUE), 4), ")"
      ), # HHI with SD in brackets
      `Diversity` = round(mean(`Diversity per Date`, na.rm = TRUE), 4), # Diversity with SD in brackets
      `L1 Norm M(SD)` = paste0(
        round(mean(`L1 Norm per Date`, na.rm = TRUE), 4),
        " (", round(sd(`L1 Norm per Date`, na.rm = TRUE), 4), ")"
      ), # L1 Norm with SD in brackets
      `L2 Norm M(SD)` = paste0(
        round(mean(`L2 Norm per Date`, na.rm = TRUE), 4),
        " (", round(sd(`L2 Norm per Date`, na.rm = TRUE), 4), ")"
      ), # L2 Norm with SD in brackets
      `Avg. Short Weights M(SD)` = paste0(
        round(mean(`Short Weights per Date`, na.rm = TRUE), 4),
        " (", round(sd(`Short Weights per Date`, na.rm = TRUE), 4), ")"
      ), # Short weights with SD in brackets
      .groups = "drop"
    )

  # Compute turnover using calculate_turnover() and map for each portfolio
  turnover_summary <- portfolio_names %>%
    purrr::map_dfr(
      ~ {
        portfolio_weights <- weights %>%
          select(stock_id, date, weight = !!sym(.x))
        turnover <- calculate_turnover(portfolio_weights, portfolio_object$actual_returns)

        tibble(
          Portfolio = .x,
          `Average Turnover` = round(mean(turnover$per_period_turnover, na.rm = TRUE), 4),
          `Total Turnover` = round(sum(turnover$per_period_turnover, na.rm = TRUE), 4)
        )
      }
    )

  # Combine results
  results <- weight_summary %>%
    left_join(turnover_summary, by = "Portfolio") %>%
    arrange(match(Portfolio, portfolio_names))

  # Print the results in a nicely formatted table if `print` is TRUE
  if (print) {
    print(knitr::kable(
      results,
      digits = 4,
      align = "c",
      format = "markdown",
      caption = "Portfolio Weights Summary with Advanced Metrics"
    ))
  }

  return(results)
}


#' Combine portfolioReturns Objects
#'
#' Combines multiple `portfolioReturns` objects into a single combined object. Validates that
#' all objects have consistent structure (either with or without benchmarks).
#'
#' @param portfolio_list A list of `portfolioReturns` objects to combine.
#'
#' @return A combined `portfolioReturns` object.
#' @export
combine_portfolioReturns <- function(portfolio_list) {
  # Validate input is a list of portfolioReturns objects
  checkmate::assert_list(portfolio_list, any.missing = FALSE)
  if (!all(sapply(portfolio_list, function(x) inherits(x, "portfolioReturns")))) {
    cli::cli_abort("All objects in `portfolio_list` must be of class `portfolioReturns`.")
  }

  # Check if all portfolioReturns objects have consistent benchmark structure
  has_benchmark <- sapply(portfolio_list, function(pf) !is.null(pf$benchmark_returns))
  if (!all(has_benchmark == has_benchmark[1])) {
    cli::cli_abort("All `portfolioReturns` objects must either have or not have a benchmark.")
  }

  # Start with the first portfolio object
  combined_portfolio <- portfolio_list[[1]]
  is_benchmark_portfolio <- has_benchmark[1]

  # Iterate through remaining portfolio objects to combine
  for (i in 2:length(portfolio_list)) {
    current_portfolio <- portfolio_list[[i]]

    # Combine models
    for (model_id in names(current_portfolio$models)) {
      # Determine the appropriate weights
      if (is_benchmark_portfolio) {
        # For benchmark portfolios, the new weights are delta weights
        new_weights <- current_portfolio$delta_weights %>%
          dplyr::select(stock_id, date, pred_weight=!!model_id)
      } else {
        # For non-benchmark portfolios, the new weights are the portfolio weights
        new_weights <- current_portfolio$weights %>%
          dplyr::select(stock_id, date, pred_weight=!!model_id)
      }

      # Add the model to the combined portfolio
      combined_portfolio <- add_weight_model(
        portfolio_object = combined_portfolio,
        model_name = current_portfolio$models[[model_id]]$model_name,
        new_weights = new_weights,
        config = current_portfolio$models[[model_id]]$config,
        postprocessing_config = current_portfolio$postprocessing_config[[model_id]]
      )
    }
  }

  # Inform the user
  cli::cli_alert_success("Combined {length(portfolio_list)} portfolioReturns objects into one.")

  return(combined_portfolio)
}



#' Plotting method for portfolioReturns S3 objects
#'
#' @param portfolio_object A portfolioReturns S3 object
#' @param type default=NULL (standrad evaluation), Alternative: tq_performance_functions
#'
#' @return A ggplot object
#'
#' @import ggplot2
#' @import tidyquant
#' @importFrom dplyr group_by mutate
#' @importFrom tidyr pivot_longer
#'
#' @export
#'
plot.portfolioReturns <- function(portfolio_object, type = NULL) {
  returns_data <- portfolio_object$portfolio_returns
  weights_data <- portfolio_object$weights
  actual_data <- portfolio_object$actual_returns
  # long_format
  returns_data_long <- returns_data %>%
    tidyr::pivot_longer(cols = -date, names_to = "portfolio", values_to = "returns") |>
    dplyr::arrange(date)
  if (is.null(type)) {
    returns_data_long %>%
      dplyr::group_by(portfolio) |>
      dplyr::mutate(cum_returns = cumprod(1+returns)-1) |>
      ggplot(aes(x = date, y = cum_returns, color = portfolio)) +
      geom_line() +
      theme_grey() +
      labs(title = "Portfolio Wealth Index", x = "Date", y = "Returns") +
      theme(legend.position = "bottom") +
      theme_tq()
  } else {
    # User specifies a PerformanceAnalytics plotting function
    xts_data <- timetk::tk_xts(returns_data)
    # Call the user-specified PerformanceAnalytics function
    plot_func <- match.fun(type)
    plot_func(xts_data)
  }
}
