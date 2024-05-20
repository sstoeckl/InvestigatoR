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
#' # Assuming 'return_prediction' has been initialized and contains actual returns
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
#
#
# plot.returnPrediction <- function(object, ...) {
#   # Example: Plotting predictions vs. errors
#   plot(object$predictions, object$prediction_errors, ...)
# }
#' Create portfolioReturns S3 object
#'
#' @param data Data frame containing stock_id, date, and the specified label column
#' @param label Column name of the label column
#'
#' @return A portfolioReturns S3 object
#'
#' @importFrom dplyr select distinct
#' @importFrom tibble tibble
#'
#' @export
#'
#' @examples
#' data <- tibble::tibble(
#'   stock_id = 1:100,
#'   date = c(rep(Sys.Date(), 50), rep(Sys.Date() + 1, 50)),
#'   return_label = runif(100)
#' )
#' # Create the returnPrediction object
#' pf <- create_portfolios(data, "return_label")
#' # Check the initial setup
#' print(pf)
create_portfolios <- function(data, label) {
  # Validate the input data contains necessary columns
  if (!("stock_id" %in% names(data)) || !("date" %in% names(data)) || !(label %in% names(data))) {
    stop("Data must contain 'stock_id', 'date', and the specified label column.")
  }

  # Prepare the tibbles
  weights <- data %>%
    dplyr::select(stock_id, date)

  actual_returns <- data %>%
    dplyr::select(stock_id, date, actual_return=!!rlang::sym(label))  # Ensure actual returns are included

  portfolio_returns <- data %>%
    dplyr::distinct(date)

  # Bundle into a list with the class 'portfolioReturns'
  structure(list(
    weight_models = list(),
    weights = weights,
    actual_returns = actual_returns,
    portfolio_returns = portfolio_returns
  ), class = "portfolioReturns")
}

#' Add a new weight model to the portfolioReturns S3 object
#'
#' @param portfolio_object A portfolioReturns object
#' @param weight_model Config/Model of the weight model
#' @param weight_config Configuration of the weight model
#' @param new_weights Data frame containing stock_id, date, and the new weights
#'
#' @return A portfolioReturns S3 object with the new weight model added
#'
#' @importFrom dplyr select distinct bind_rows left_join arrange
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Assuming 'pf' is your portfolio_object already created
#' weight_function <- "ensemble"  # Example model function
#' weight_config <- list(ensemble_list=c("ols_1","xgb_1","xgb_2"), ensemble_weights=c(0.3,0.3,0.4))
#' new_weights <- data.frame(stock_id = 1:100, date = c(rep(Sys.Date(), 50), rep(Sys.Date() + 1, 50)), weights = runif(100))
#' # Add the new model and its weights to the portfolio
#' pf <- add_weight_model(pf, weight_function, weight_config, new_weights)
#' print(pf)
#' }
add_weight_model <- function(portfolio_object, weight_model, weight_config, new_weights) {
  # Check that new_weights contains the necessary columns
  if (!("stock_id" %in% names(new_weights)) || !("date" %in% names(new_weights))) {
    stop("new_weights must contain 'stock_id' and 'date' columns.")
  }

  # Generate a unique identifier for the new weight model
  model_name <- weight_model
  existing_ids <- names(portfolio_object$weights)
  counter <- find_largest_number(strings = existing_ids, paste0(names(new_weights)[3],"_",model_name)) +1
  model_id <- paste0(names(new_weights)[3], "_", model_name, "_", counter)

  # Add model details
  portfolio_object$weight_models[[model_id]] <- list(
    function_name = weight_model,
    config = weight_config,  # Assuming config can be derived from the function environment
    datetime = Sys.time()
  )
  # Prepare new_predictions by renaming the prediction column to the model identifier
  names(new_weights)[3] <- model_id

  # Join new weights with existing weights
  portfolio_object$weights <- portfolio_object$weights |>
    dplyr::left_join(new_weights, by = c("stock_id", "date"))

  # Update portfolio returns
  new_portfolio_returns <- portfolio_object$weights |>
    dplyr::select(stock_id, date, weights=!!model_id) |>
    dplyr::left_join(portfolio_object$actual_returns, by = c("stock_id", "date")) |>
    dplyr::group_by(date) |>
    dplyr::summarise(portfolio_return = sum(actual_return*weights)) |>
    dplyr::arrange(date)
      # portfolio_return = weighted.mean(actual_returns, weights))

  names(new_portfolio_returns)[names(new_portfolio_returns) == "portfolio_return"] <- model_id
  portfolio_object$portfolio_returns <- portfolio_object$portfolio_returns %>%
    dplyr::left_join(new_portfolio_returns, by = "date")

  return(portfolio_object)
}
#' Define the summary method for 'portfolioReturns' class
#'
#' @param portfolio_object A portfolioReturns object
#' @param type default=NULL (standrad evaluation), Alternative: tq_performance_functions
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
# library(cowplot)   # Plot grid management
# g1 <- tibble(date = t_oos,
#              benchmark = cumprod(1+portf_returns[,1]),
#              ml_based = cumprod(1+portf_returns[,2])) %>%
#   gather(key = strat, value = value, -date) %>%
#   ggplot(aes(x = date, y = value, color = strat)) + geom_line() +theme_grey()
# g2 <- tibble(year = lubridate::year(t_oos),
#              benchmark = portf_returns[,1],
#              ml_based = portf_returns[,2]) %>%
#   gather(key = strat, value = value, -year) %>%
#   group_by(year, strat) %>%
#   summarise(avg_return = mean(value)) %>%
#   ggplot(aes(x = year, y = avg_return, fill = strat)) +
#   geom_col(position = "dodge") + theme_grey()
# plot_grid(g1,g2, nrow = 2)
