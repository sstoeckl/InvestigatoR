#' Create Weights Based on Quantile Constraints
#'
#' @param return_predictions Tibble containing `stock_id`, `date`, and various prediction columns.
#' @param errors Tibble containing `stock_id`, `date`, and various error columns (not used in this function).
#' @param constraints List of constraints for weight calculation. If `NULL`, default constraints are used.
#'  - `quantiles`: List with `long` and `short` quantiles (default: `list(long = 0.10, short = 0.10)`).
#'  - `allow_short_sale`: Logical indicating if short sales are allowed (default: `TRUE`).
#'  - `min_weight`: Numeric minimum weight per position (default: `-0.1`).
#'  - `max_weight`: Numeric maximum weight per position (default: `0.1`).
#'  - `b`: Numeric multiplier for sum constraints (default: `10`).
#'
#' @return Tibble with `stock_id`, `date`, and `weight` for each prediction column.
#'
#' @importFrom dplyr arrange group_by mutate ungroup select
#' @importFrom purrr map reduce
#' @export
quantile_weights <- function(return_predictions, errors, constraints = NULL) {

  # Define default constraints if none provided
  if (is.null(constraints)) {
    constraints <- list(
      quantiles = list(long = 0.10, short = 0.10),
      allow_short_sale = TRUE,
      min_weight = -0.1,  # Allows short sales up to -10%
      max_weight = 0.1,   # Max weight per position
      b = 10              # Max weight sum in both legs
    )
  }

  # Input Validation
  required_cols <- c("stock_id", "date")
  if (!all(required_cols %in% colnames(return_predictions))) {
    stop("return_predictions must contain 'stock_id' and 'date' columns.")
  }

  # Generate weights for each prediction column (excluding 'stock_id' and 'date')
  weight_list <- purrr::map(colnames(return_predictions)[!(colnames(return_predictions) %in% required_cols)], function(pred_col) {
    pred_data <- return_predictions %>%
      select(stock_id, date, pred = all_of(pred_col))

    if (constraints$allow_short_sale) {
      # Generate quantile portfolios with short sales
      weights <- pred_data %>%
        arrange(stock_id, date) %>%
        group_by(date) %>%
        mutate(
          quantile_long = quantile(pred, probs = 1 - constraints$quantiles$long, na.rm = TRUE),
          quantile_short = quantile(pred, probs = constraints$quantiles$short, na.rm = TRUE),
          weight = case_when(
            pred >= quantile_long ~ 1,
            pred <= quantile_short ~ -1,
            TRUE ~ 0
          )
        ) %>%
        mutate(
          weight = pmax(weight, constraints$min_weight),
          weight = pmin(weight, constraints$max_weight)
        ) %>%
        ungroup() %>%
        mutate(short = ifelse(weight < 0, 1, 0)) %>%
        group_by(date, short) %>%
        mutate(weight = weight / sum(weight) * constraints$b) %>%
        ungroup() %>%
        select(stock_id, date, weight)
    } else {
      # Generate quantile portfolios without short sales
      weights <- pred_data %>%
        arrange(stock_id, date) %>%
        group_by(date) %>%
        mutate(
          quantile_long = quantile(pred, probs = 1 - constraints$quantiles$long, na.rm = TRUE),
          weight = ifelse(pred >= quantile_long, 1, 0)
        ) %>%
        mutate(
          weight = pmax(weight, 0)  # Ensure no short sales
        ) %>%
        group_by(date) %>%
        mutate(
          weight = weight / sum(weight) * constraints$b
        ) %>%
        ungroup() %>%
        select(stock_id, date, weight)
    }

    return(weights)
  })

  # Combine all weight tibbles into one
  combined_weights <- purrr::reduce(weight_list, left_join, by = c("stock_id", "date"))

  return(combined_weights)
}

#' Create Weights Based on Ensemble Methods
#'
#' @param return_predictions Tibble containing `stock_id`, `date`, and various prediction columns.
#' @param errors Tibble containing `stock_id`, `date`, and various error columns.
#' @param method Character. Method to use for ensemble weights. Possible values are:
#'  - "simple_average": Simple average of predictions.
#'  - "weighted_average": Weighted average based on the inverse of average errors.
#'  - "error_covariance": Using the covariance of errors to calculate weights.
#'
#' @return Numeric vector of ensemble predictions.
#'
#' @importFrom dplyr select
#' @importFrom purrr reduce
#' @export
ensemble_weights <- function(return_predictions, errors, method = "simple_average") {

  # Validate inputs
  if (!is.data.frame(return_predictions) || !is.data.frame(errors)) {
    stop("Both 'return_predictions' and 'errors' should be data frames.")
  }

  # Ensure predictions and errors align
  if (nrow(return_predictions) != nrow(errors) || ncol(return_predictions) != ncol(errors)) {
    stop("Predictions and errors must have the same dimensions.")
  }

  # Remove 'stock_id' and 'date' columns for calculations
  preds <- return_predictions %>%
    select(-stock_id, -date)

  errs <- errors %>%
    select(-stock_id, -date)

  # Define ensemble prediction based on method
  if (method == "simple_average") {
    # Simple average of predictions
    ensemble_prediction <- rowMeans(preds, na.rm = TRUE)
  } else if (method == "weighted_average") {
    # Weighted average based on the inverse of average errors
    average_errors <- rowMeans(errs, na.rm = TRUE)
    weights <- 1 / average_errors
    weights <- weights / sum(weights)  # Normalize weights
    ensemble_prediction <- as.numeric(as.matrix(preds) %*% weights)
  } else if (method == "error_covariance") {
    # Using the covariance of errors to calculate weights
    E <- as.matrix(errs)
    cov_E <- cov(E, use = "pairwise.complete.obs")

    # Check if covariance matrix is invertible
    if (det(cov_E) == 0) {
      stop("Covariance matrix of errors is singular and cannot be inverted.")
    }

    inv_cov_E <- solve(cov_E)
    one_vector <- rep(1, ncol(E))
    weights <- inv_cov_E %*% one_vector
    weights <- weights / sum(weights)  # Normalize weights
    ensemble_prediction <- as.numeric(as.matrix(preds) %*% weights)
  } else {
    stop("Specified method is not supported. Choose from 'simple_average', 'weighted_average', or 'error_covariance'.")
  }

  return(ensemble_prediction)
}

