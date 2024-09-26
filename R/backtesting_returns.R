#' Backtesting Returns Function
#'
#' This function utilizes machine learning models to predict asset returns, which are subsequently used for portfolio trading strategies.
#'
#' @param data ML dataset (tibble/data.frame) in long format containing the features, return label, stock IDs (first column), and dates (second column).
#'   The dataset should be free of missing values and, if necessary, balanced in terms of the number of stocks available at each point in time.
#' @param return_prediction_object An object of class `returnPrediction` to store the predictions. Default is `NULL`, which creates a new object.
#'   If an existing object is provided and `append = TRUE`, new predictions are added. If `append = FALSE`, the object is overwritten.
#' @param return_label The prediction label to be used for the ML model. It should be appropriately shifted (e.g., the label for date `t` should correspond to returns from date `t+1`).
#' @param features A character vector of feature names to be used for the ML model.
#' @param rolling Logical indicating whether to use a rolling window approach (`TRUE`) or an expanding window approach (`FALSE`) for prediction.
#' @param window_size The size of the window for the rolling window approach. Can be specified in number of time steps or as a period (e.g., "1 year", "6 months").
#'   If `rolling = FALSE`, this represents the starting window size for the expanding window approach.
#' @param step_size The interval at which the prediction window moves forward (e.g., "1 month", "15 days"). Default is "1 month".
#' @param offset The size of the data to be left unused between training and prediction to avoid look-ahead bias. Can be specified in time steps or periods (e.g., "1 month"). Default is "0".
#' @param in_sample Logical indicating whether to provide in-sample predictions for the training period (+ offset). Default is `TRUE`.
#' @param ml_config A list containing configurations for the ML models. Each element should specify the prediction function and its respective configurations.
#' @param append Logical indicating whether to append the predicted returns to the original dataset (`TRUE`) or overwrite the existing `returnPrediction` object (`FALSE`).
#' @param num_cores The number of cores to be used for parallel processing. If set to `NULL`, ML iterations are performed sequentially.
#' @param verbose Logical indicating whether to display detailed progress messages. Default is `FALSE`.
#'
#' @return An S3 object of class `returnPrediction` containing models, predictions, actual returns, and errors.
#'
#' @importFrom dplyr bind_rows distinct arrange pull mutate rename select all_of
#' @importFrom tibble tibble
#' @import future
#' @importFrom furrr future_map_dfr
#' @import checkmate
#' @importFrom cli cli_inform cli_alert_info cli_alert_success cli_alert_warning cli_progress_bar cli_progress_update cli_progress_done
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data(data_ml)
#' data_subset <- data_ml |>  filter(stock_id<=30)
#' return_label <- "R1M_Usd"
#' features <- c("Div_Yld", "Eps", "Mkt_Cap_12M_Usd", "Mom_11M_Usd", "Ocf", "Pb", "Vol1Y_Usd")
#' rolling <- TRUE
#' window_size <- "5 years"
#' step_size <- "3 months"
#' offset <- "1 year"
#' in_sample <- TRUE
#' ml_config <- list(
#'   ols_pred = list(pred_func = "ols_pred", config = list()),
#'   xgb_pred = list(
#'     pred_func = "xgb_pred",
#'     config1 = list(nrounds = 100, max_depth = 3, eta = 0.3, objective = "reg:squarederror"),
#'     config2 = list(nrounds = 100, max_depth = 4, eta = 0.1, objective = "reg:squarederror")
#'   )
#' )
#' rp <- backtesting_returns(
#'   data = data_subset,
#'   return_prediction_object = NULL,
#'   return_label = return_label,
#'   features = features,
#'   rolling = rolling,
#'   window_size = window_size,
#'   step_size = step_size,
#'   offset = offset,
#'   in_sample = in_sample,
#'   ml_config = ml_config,
#'   append = FALSE,
#'   num_cores = NULL,
#'   verbose = TRUE
#' )
#' }
backtesting_returns <- function(data, return_prediction_object = NULL, return_label, features,
                                rolling = TRUE, window_size, step_size = "1 month", offset = "0",
                                in_sample = TRUE, ml_config, append = FALSE, num_cores = NULL,
                                verbose = FALSE) {

  # Input Validation using checkmate
  if (verbose) {
    cli::cli_inform("Starting input validation...")
  }

  # Validate 'data'
  checkmate::assert_data_frame(data, min.rows = 1, any.missing = FALSE, .var.name = "data")

  # Validate 'return_prediction_object'
  if (!is.null(return_prediction_object)) {
    checkmate::assert_class(return_prediction_object, "returnPrediction", .var.name = "return_prediction_object")
  }

  # Validate 'return_label'
  checkmate::assert_string(return_label, min.chars = 1, .var.name = "return_label")
  if (!return_label %in% colnames(data)) {
    cli::cli_abort("The label '{return_label}' is missing from the dataset.")
  }

  # Validate 'features'
  checkmate::assert_character(features, min.len = 1, .var.name = "features")
  missing_features <- setdiff(features, colnames(data))
  if (length(missing_features) > 0) {
    cli::cli_abort("The following features are missing from the dataset: {paste(missing_features, collapse = ', ')}")
  }

  # Validate 'rolling'
  checkmate::assert_flag(rolling, .var.name = "rolling")

  # Validate 'window_size'
  checkmate::assert_string(window_size, min.chars = 1, .var.name = "window_size")

  # Validate 'step_size'
  checkmate::assert_string(step_size, min.chars = 1, .var.name = "step_size")

  # Validate 'offset'
  checkmate::assert_string(offset, min.chars = 1, .var.name = "offset")

  # Validate 'in_sample'
  checkmate::assert_flag(in_sample, .var.name = "in_sample")

  # Validate 'ml_config'
  checkmate::assert_list(ml_config, min.len = 1, .var.name = "ml_config")

  # Validate 'append'
  checkmate::assert_flag(append, .var.name = "append")

  # Validate 'num_cores'
  if (!is.null(num_cores)) {
    checkmate::assert_integer(num_cores, len = 1, lower = 1, .var.name = "num_cores")
  }

  # Validate 'verbose'
  checkmate::assert_flag(verbose, .var.name = "verbose")

  if (verbose) {
    cli::cli_alert_success("Input validation passed.")
  }

  ## Data Preparation
  if (verbose) {
    cli::cli_inform("Renaming first two columns to 'stock_id' and 'date'...")
  }
  data <- data %>% dplyr::rename(stock_id = 1, date = 2)

  if (verbose) {
    cli::cli_inform("Converting 'date' column to Date type...")
  }
  data <- data %>%
    dplyr::mutate(date = as.Date(date, tryFormats = c("%Y-%m-%d", "%d/%m/%Y", "%m/%d/%Y")))

  # Check date conversion
  if (any(is.na(data$date))) {
    cli::cli_abort("Date conversion failed. Please check the date format in the 'date' column.")
  }

  if (verbose) {
    cli::cli_inform("Checking for missing values...")
  }
  # Check for NAs
  missing_values <- sum(is.na(data))
  if (missing_values > 0) {
    cli::cli_abort("The dataset contains {missing_values} missing values. Please handle missing data before proceeding.")
  }

  if (verbose) {
    cli::cli_inform("Checking for duplicate rows...")
  }
  # Check for duplicate rows
  duplicate_rows <- nrow(data) - nrow(dplyr::distinct(data))
  if (duplicate_rows > 0) {
    cli::cli_abort("The dataset contains {duplicate_rows} duplicate rows. Please remove duplicates before proceeding.")
  }

  if (verbose) {
    cli::cli_inform("Extracting and arranging unique dates...")
  }
  # Extract and arrange unique dates
  dates <- data %>%
    dplyr::select(date) %>%
    dplyr::distinct() %>%
    dplyr::arrange(date) %>%
    dplyr::pull(date)

  if (verbose) {
    cli::cli_inform("Generating training and prediction indices based on offset and window settings...")
  }
  # Generate training and prediction indices
  indices <- select_dates_by_offset(dates, window_size, step_size, offset, rolling)

  # Assuming select_dates_by_offset returns a data frame with columns: training_start, training_end, prediction_start, prediction_end
  indices <- dplyr::bind_rows(
    tibble::tibble(
      training_start = indices$training_start[1],
      training_end = indices$training_end[1],
      prediction_start = indices$training_start[1],
      prediction_end = indices$prediction_start[1],
      prediction_phase = "IS"
    ),
    indices
  )

  if (verbose) {
    cli::cli_inform("Subsetting data for return label and features...")
  }
  # Subset data to include only necessary columns
  data_subset <- data %>%
    dplyr::select(stock_id, date, dplyr::all_of(return_label), dplyr::all_of(features))

  if (verbose) {
    if (is.null(return_prediction_object)) {
      cli::cli_inform("Initializing a new 'returnPrediction' object...")
    } else {
      cli::cli_inform("Appending to the existing 'returnPrediction' object...")
    }
  }

  # Initialize or append to returnPrediction object
  if (is.null(return_prediction_object)) {
    return_prediction_object <- create_return_prediction(data_subset, return_label)
    if (verbose) {
      cli::cli_alert_success("Successfully created a new 'returnPrediction' object.")
    }
  } else {
    if (verbose) {
      cli::cli_alert_success("Successfully appended to the existing 'returnPrediction' object.")
    }
  }

  if (verbose) {
    cli::cli_inform("Creating prediction configuration...")
  }
  # Create prediction configuration
  pred_config <- list(
    return_label = return_label,
    features = features,
    rolling = rolling,
    window_size = window_size,
    step_size = step_size,
    offset = offset,
    in_sample = in_sample,
    indices = indices
  )

  if (verbose) {
    cli::cli_inform("Setting up parallel processing...")
  }
  # Set up parallel processing
  if (!is.null(num_cores)) {
    future::plan(multicore, workers = num_cores)
    options(future.seed = TRUE)
    if (verbose) {
      cli::cli_alert_info("Parallel processing enabled with {num_cores} cores.")
    }
  } else {
    future::plan("sequential")
    options(future.seed = TRUE)
    if (verbose) {
      cli::cli_alert_info("Sequential processing enabled.")
    }
  }

  if (verbose) {
    cli::cli_inform("Starting model processing...")
  }

  # Loop through each model in ml_config
  for (i in seq_along(ml_config)) {
    model_name <- names(ml_config)[i]
    model_specs <- ml_config[[i]]

    # Validate presence of 'pred_func'
    if (is.null(model_specs$pred_func)) {
      cli::cli_abort("Model '{model_name}' does not have a 'pred_func' specified in 'ml_config'.")
    }

    model_function <- model_specs$pred_func
    config_names <- setdiff(names(model_specs), "pred_func")

    if (length(config_names) == 0) {
      cli::cli_alert_warning("Model '{model_name}' does not have any configurations. Skipping.")
      next
    }

    cli::cli_alert_info("Processing model {i}/{length(ml_config)}: {model_name}")

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
      back_test <- furrr::future_map_dfr(
        map_indices,
        ~ {
          result <- retpred_map(.x, data_subset, indices, model_function, model_config)
          if (verbose) {
            cli::cli_progress_update(id = pb)
          }
          return(result)
        },
        .options = furrr::furrr_options(seed = TRUE)
      ) %>%
        dplyr::rename(prediction = pred_return)

      # Finish progress bar
      if (verbose) {
        cli::cli_progress_done(id = pb)
      }

      # Append predictions to returnPrediction object
      model_config_plus <- model_config
      model_config_plus[["pred_config"]] <- pred_config

      return_prediction_object <- add_model_prediction(return_prediction_object, model_function, model_config_plus, back_test)

      if (verbose) {
        cli::cli_alert_success("    Successfully added predictions for configuration '{config_name}' of model '{model_name}'.")
      }
    }
  }

  if (verbose) {
    cli::cli_alert_success("Completed all model processing.")
  }

  # Reset parallel processing to sequential
  future::plan("sequential")
  if (verbose) {
    cli::cli_alert_info("Parallel processing plan reset to sequential.")
  }

  if (verbose) {
    cli::cli_inform("Returning the 'returnPrediction' object.")
  }

  return(return_prediction_object)
}
