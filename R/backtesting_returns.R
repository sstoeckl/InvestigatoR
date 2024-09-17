#' Backtesting Returns Function
#'
#' This function uses machine learning models to predict returns that will later be used for a trading strategy.
#'
#' @param data ML dataset (tibble/data.frame) in long format that should contain the features and the `return_label` as well as the `stock_ids` (first column) and
#' dates (second column). For most ML algorithms to work, this dataset should not contain missing values. Sometimes it needs
#' to be balanced in terms of the number of stocks available at each point in time.
#' @param return_prediction_object An object of class `returnPrediction` that should be used to store the predictions. Default (`NULL`) creates a new one.
#' In case an existing object is passed, given `append = TRUE`, new predictions are added. If `append = FALSE`, the object is overwritten.
#' @param return_label The prediction label that should be used for the ML model. It should already be appropriately shifted (and date the label should be from date `t+1`).
#' @param features A vector of features that should be used for the ML model.
#' @param rolling If `TRUE`, the function will use a rolling window approach to predict the returns. If `FALSE`, the function will use an expanding window approach.
#' @param window_size (Either in number of time steps or in years or months as `"1 year"` or `"1 month"`) The size of the window that should be used for the
#' rolling window approach. If `rolling = FALSE`, this is the starting window for the expanding window approach.
#' @param step_size The amount of time the prediction window should be moved forward. Default is `"1 month"`. If set to `"3 months"`, returns will be predicted for `t`, `t+1`, and `t+2`
#' (corresponding to `t+1`, `t+2`, and `t+3`) in the original dataset. Only then will the ML model be retrained.
#' @param offset (Either in number of time steps or in years or months as `"1 year"` or `"1 month"`) The size of data that should be left unused between training data
#' and prediction (to avoid look-ahead bias). Default is `"0 months"`.
#' @param in_sample If `TRUE`, the function will also provide (in-sample) predictions for the training period (plus offset).
#' @param ml_config A list that contains the configuration for the ML models. It should contain the following elements:
#' @param append If `TRUE`, the function will append the predicted returns to the original dataset. If `FALSE`, the function will return a new dataset that contains the
#' predicted returns.
#' @param num_cores The number of cores that should be used for parallel processing. If set to `NULL`, the ML iterations will be done sequentially.
#' @param verbose If `TRUE`, detailed messages will be printed. Default is `TRUE`.
#' @param seed integer. Seed for random number generation
#'
#' @return An S3 object of class `returnPrediction` that contains all the information associated with the backtesting results. It includes information on the used models (`rp$models`), the predictions (`rp$predictions`), the actual returns (`rp$actual_returns`), and the errors (`rp$errors`).
#'
#' @importFrom dplyr bind_rows distinct arrange pull mutate select rename
#' @importFrom tibble tibble
#' @importFrom checkmate assert_data_frame assert_string assert_character assert_flag assert_list assert_number
#' @import future
#' @importFrom furrr future_map_dfr
#' @import progressr
#' @import cli
#' @importFrom purrr list_modify
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data(data_ml)
#' return_label <- "R1M_Usd"
#' features <- c("Div_Yld", "Eps", "Mkt_Cap_12M_Usd", "Mom_11M_Usd", "Ocf", "Pb", "Vol1Y_Usd")
#' rolling <- FALSE
#' window_size <- "5 years"
#' step_size <- "1 month"
#' offset <- "1 month"
#' in_sample <- TRUE
#' ml_config <- list(
#'   ols_pred = list(pred_func = "ols_pred", config = list()),
#'   xgb_pred = list(
#'     pred_func = "xgb_pred",
#'     config1 = list(nrounds = 10, max_depth = 3, eta = 0.3, objective = "reg:squarederror"),
#'     config2 = list(nrounds = 10, max_depth = 3, eta = 0.1, objective = "reg:squarederror")
#'   )
#' )
#' rp <- backtesting_returns(
#'   data = data_ml,
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
#'   verbose = TRUE,
#'   seed=123
#' )
#' }
backtesting_returns <- function(data,
                                return_prediction_object = NULL,
                                return_label,
                                features,
                                rolling = TRUE,
                                window_size,
                                step_size = "1 month",
                                offset = "0 months",
                                in_sample = TRUE,
                                ml_config,
                                append = FALSE,
                                num_cores = NULL,
                                verbose = TRUE,
                                seed = 123) {

  # Input Validation
  checkmate::assert_data_frame(data, min.rows = 1, min.cols = 3)
  checkmate::assert_string(return_label)
  checkmate::assert_character(features, min.len = 1)
  checkmate::assert_flag(rolling)
  checkmate::assert_character(window_size, pattern = "^\\d+\\s+(year|years|month|months)$",
                              null.ok = FALSE,
                              any.missing = FALSE)
  checkmate::assert_character(step_size, pattern = "^\\d+\\s+(year|years|month|months)$",
                              null.ok = FALSE,
                              any.missing = FALSE)
  checkmate::assert_character(offset, pattern = "^\\d+\\s+(year|years|month|months)$",
                              null.ok = FALSE,
                              any.missing = FALSE)
  checkmate::assert_flag(in_sample)
  checkmate::assert_list(ml_config, types = "list")
  checkmate::assert_flag(append)
  checkmate::assert_number(num_cores, lower = 1, null.ok = TRUE)
  checkmate::assert_flag(verbose)

  # Load required packages
  library(dplyr)
  library(tibble)
  library(checkmate)
  library(furrr)
  library(progressr)
  library(cli)
  library(purrr)

  # Initialize progress handlers if verbose and progressr is available
  if (verbose) {
    handlers("progress")
  } else {
    handlers("none")
  }

  # Start progress handler
  with_progress({
    p <- progressor(along = seq_along(ml_config) * length(ml_config[[1]]) )  # Adjust based on number of models and configs

    # Data Preprocessing
    cli::cli_inform("Preprocessing data...")
    data <- data %>%
      rename(stock_id = 1, date = 2) %>%
      mutate(date = as.Date(date, tryFormats = c("%Y-%m-%d", "%d/%m/%Y", "%m/%d/%Y")))

    # Check if date conversion was successful
    if (any(is.na(data$date))) {
      cli::cli_abort("Date conversion failed. Please check the date format.")
    }

    # Check for the presence of return_label and features
    if (!return_label %in% colnames(data)) {
      cli::cli_abort("The label '{return_label}' is missing from the dataset.")
    }
    missing_features <- setdiff(features, colnames(data))
    if (length(missing_features) > 0) {
      cli::cli_abort("The following features are missing from the dataset: {paste(missing_features, collapse = ', ')}")
    }

    # Check for NAs
    missing_values <- sum(is.na(data))
    if (missing_values > 0) {
      cli::cli_abort("The dataset contains {missing_values} missing values. Please handle missing data.")
    }

    # Check for duplicate rows
    duplicate_rows <- nrow(data) - nrow(dplyr::distinct(data))
    if (duplicate_rows > 0) {
      cli::cli_abort("The dataset contains {duplicate_rows} duplicate rows. Please remove duplicates.")
    }

    # Extract dates
    dates <- data %>%
      select(date) %>%
      distinct() %>%
      arrange(date) %>%
      pull(date)

    # Generate tibble with training/prediction start & end dates
    indices <- select_dates_by_offset(dates, window_size, step_size, offset, rolling)

    # Assuming select_dates_by_offset returns a data frame with training_start, training_end, prediction_start, prediction_end
    # Append the first index as in-sample if required
    if (in_sample) {
      indices <- bind_rows(
        tibble(
          training_start = indices$training_start[1],
          training_end = indices$training_end[1],
          prediction_start = indices$training_start[1],
          prediction_end = indices$prediction_start[1],
          prediction_phase = "IS"
        ),
        indices
      )
    }

    # Subset data to desired label and features (ensure label is in position "3")
    data_subset <- data %>%
      select(stock_id, date, all_of(return_label), all_of(features))

    # Create returnPrediction object if not provided
    if (is.null(return_prediction_object)) {
      cli::cli_inform("Creating a new returnPrediction object.")
      return_prediction_object <- create_return_prediction(data_subset, return_label)
    }

    # Create prediction config
    pred_config <- list(
      return_label = return_label,
      features = features,
      rolling = rolling,
      window_size = window_size,
      step_size = step_size,
      offset = offset,
      in_sample = in_sample,
      indices = indices,
      seed = seed  # Pass seed to config
    )

    # Set up parallel processing
    if (!is.null(num_cores)) {
      future::plan(multicore, workers = num_cores)
      on.exit(future::plan(sequential), add = TRUE)
    } else {
      future::plan(sequential)
    }

    # Loop through the models in the ml_config list with progress
    for (i in seq_along(ml_config)) {
      model_name <- names(ml_config)[i]
      cli::cli_alert_info("Processing model {i}/{length(ml_config)}: {model_name}")
      p()

      model_specs <- ml_config[[model_name]]

      # Extract prediction function and configs
      pred_func <- model_specs$pred_func
      configs <- model_specs[names(model_specs) != "pred_func"]

      for (j in seq_along(configs)) {
        config_name <- names(configs)[j]
        config <- configs[[j]]
        cli::cli_alert_info("  Processing config {j}/{length(configs)}: {config_name} with function {pred_func}")
        p()

        # Merge seed into config if necessary
        config_with_seed <- purrr::list_modify(config, seed = seed)

        # Use future_map_dfr for parallel processing and combine results
        predictions <- tryCatch({
          furrr::future_map_dfr(
            seq(nrow(indices)),
            ~ retpred_map(.x, data_subset, indices, pred_func, config_with_seed),
            .options = furrr::furrr_options(seed = TRUE,    packages = "InvestigatoR"  # Ensure the package is loaded in workers
                                            )
          )
        }, error = function(e) {
          cli::cli_alert_danger("Error in model '{model_name}' with config '{config_name}': {e$message}")
          NULL
        })

        # Check if predictions were successful
        if (!is.null(predictions)) {
          predictions <- predictions %>%
            rename(prediction = pred_return)

          # Add to return_prediction_object with error handling
          tryCatch({
            model_config_plus <- purrr::list_modify(config, pred_config = pred_config)
            return_prediction_object <- add_model_prediction(return_prediction_object, pred_func, model_config_plus, predictions)
          }, error = function(e) {
            cli::cli_alert_danger("Failed to add predictions to returnPrediction object: {e$message}")
          })
        }

        p()
      }
    }

    # End of progress handler
  })

  # Return the returnPrediction object
  return(return_prediction_object)
}
