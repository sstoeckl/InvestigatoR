#' This function is used to use ML to predict the returns that will later be used for a trading strategy
#'
#' @param data ML dataset (tibble/data.frame) in long format that should contain the features and the return_label as well as the stock_ids (first column) and
#' dates (second column). FOr most ML algorithms to work this data set should not contain missing values. Sometimes it needs
#' to be balanced in terms of number of stocks available at each point in time.
#' @param return_prediction_object an object of class returnPrediction that should be used to store the predictions. Defaul(NULL) creates a new one.
#' In case an existing object is passed, given append=TRUE, new predictions are added. If append=FALSE, the object is overwritten.
#' @param return_label the prediction label that should be used for the ML model. It should already be appropriately shifted (and date t the label should be from date t+1).
#' @param features a vector of features that should be used for the ML model.
#' @param rolling if TRUE, the function will use a rolling window approach to predict the returns. If FALSE, the function will use an expanding window approach.
#' @param window_size (either in number of time steps or in years or months as "1 year" or "1 month"!) the size of the window that should be used for the
#' rolling window approach. if rolling=FALSE this is the starting window for the expoaning window approach
#' @param step_size the amount of days the prediction window should be moved forward. Default is 1. If (e.g.) set to three, returns will be predicted for t, t+1 and t+2
#' (corresponding to t+1 and t+2 and t+3) in the original datset. Only then will the ML model be retrained.
#' @param offset (either in number of time steps or in years or months as "1 year" or "1 month"!) the size of data that should be left unused between training data
#'  and prediction (to avoid look-ahead bias). Default is 0.
#' @param in_sample if TRUE, the function will also provide (in-sample) predictions for the training period (+ offfest)
#' @param ml_config a list that contains the configuration for the ML model. It should contain the following elements:
#' @param append if TRUE, the function will append the predicted returns to the original dataset. If FALSE, the function will return a new dataset that contains the
#' predicted returns.
#' @param verbose num_cores the number of cores that should be used for parallel processing. If set to NULL the ML iterations will be done sequentially.
#'
#' @return a tibble with the stock_id, date and the predicted returns
#'
#' @importFrom dplyr bind_rows distinct arrange pull
#' @importFrom tibble tibble
#' @import future
#' @importFrom furrr future_map
#'
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data(data_ml)
#' data <- data_ml
#' return_label <- "R1M_Usd"
#' features <- c("Div_Yld", "Eps", "Mkt_Cap_12M_Usd", "Mom_11M_Usd", "Ocf", "Pb", "Vol1Y_Usd")
#' rolling <- TRUE; window_size= "5 years"; step_size = "3 months"; offset = "1 year"; in_sample = TRUE
#' ml_config <- list(ols_pred = list(pred_func="ols_pred", config=list()),
#'                   xgb_pred = list(pred_func="xgb_pred", config1=list(nrounds=100, max_depth=3, eta=0.3, objective="reg:squarederror"),
#'                                                     config2=list(nrounds=100, max_depth=4, eta=0.1, objective="reg:squarederror")))
#' rp <- backtesting_returns(data=data, return_prediction_object=NULL,
#'   return_label, features, rolling, window_size, step_size, offset, in_sample, ml_config, append=FALSE, num_cores=NULL)
#'
#' }
backtesting_returns <- function(data, return_prediction_object=NULL, return_label, features,
                                rolling = TRUE, window_size, step_size = 1, offset = 0, in_sample = TRUE,
                                ml_config, append = FALSE, num_cores = NULL) {
  ## Check inputs for consistency
  # data
  data <- data |> dplyr::rename(stock_id=1, date=2)
  # format date column as date, stop with error message if not possible
  data <- data %>%
    dplyr::mutate(date = as.Date(date, tryFormats = c("%Y-%m-%d", "%d/%m/%Y", "%m/%d/%Y")))
    # Check if the date conversion was successful
    if (any(is.na(data$date))) {
      stop("Date conversion failed. Please check the date format.")
    }
  # Check for the presence of return label and features
  if (!return_label %in% colnames(data)) {
    stop("The label 'return' is missing from the dataset.")
  }
  missing_features <- setdiff(features, colnames(data))
  if (length(missing_features) > 0) {
    stop(paste("The following features are missing from the dataset:", paste(missing_features, collapse = ", ")))
  }
  # check for NAs
  missing_values <- sum(is.na(data))
  if (missing_values > 0) {
    stop(paste("The dataset contains", missing_values, "missing values. Please handle missing data."))
  }
  # check for duplicate rows
  duplicate_rows <- nrow(data) - nrow(distinct(data))
  if (duplicate_rows > 0) {
    stop(paste("The dataset contains", duplicate_rows, "duplicate rows. Please remove duplicates."))
  }
  ### Now comes the main task
  # Extract dates
  dates <- data %>% dplyr::select(date) %>% dplyr::distinct() %>% dplyr::arrange(date) %>% dplyr::pull(date)

  # Generate tibble with training/prediction start & end dates
  indices <- select_dates_by_offset(dates, window_size, step_size, offset, rolling)
  indices <- dplyr::bind_rows(tibble(training_start=indices$training_start[1], training_end=indices$training_end[1],
                              prediction_start=indices$training_start[1], prediction_end=indices$prediction_start[1],
                              prediction_phase="IS"),
                       indices)


  # subset data to desired label and features (make sure, label is in position "3")
  data_subset <- data %>% dplyr::select(stock_id, date, dplyr::all_of(return_label), dplyr::all_of(features))

  # based on data subset check (and if necessary create returnPrediction ob ject
  # return prediction object
  if (is.null(return_prediction_object)) {
    return_prediction_object <- create_return_prediction(data_subset, return_label)
  }
  # create prediction config (can change from run to run, is therefore saved with the model
  pred_config <- list(
    return_label = return_label,
    features = features,
    rolling = rolling,
    window_size = window_size,
    step_size = step_size,
    offset = offset,
    in_sample = in_sample,
    indices=indices
  )

  # set up parallel processing
  if (!is.null(num_cores)) {
    future::plan(multicore, workers = num_cores)
    options(future.seed = TRUE)
  } else {
    future::plan("sequential")
    options(future.seed = TRUE)
  }
  # now we loop through the models in the ml_config list
  for (i in 1:length(ml_config)) {
    cat("Currently processing model", i, "named",names(ml_config)[i], "of", length(ml_config), "\n")
    # extract model name and config
    model_name <- names(ml_config)[i]
    # we loop over the number of configs per model
    for (j in 2:length(ml_config[[model_name]])){
      cat("Specifically processing config", j-1,"with prediction function",ml_config[[model_name]]$pred_func, "of", length(ml_config[[model_name]])-1, "\n")
    model_specs <- ml_config[[model_name]]
    # extract model function and config
    model_function <- model_specs$pred_func
    model_config <- model_specs[[j]]
    # map over the times in indices using port_map
    map_indices <- seq(nrow(indices))

    back_test <- map_indices[1] %>%
      furrr::future_map(retpred_map, data_subset, indices, model_function, model_config) %>%
      dplyr::bind_rows() |>
      dplyr::rename(prediction=pred_return)
    # add to return prediction object
    model_config_plus <- model_config
    model_config_plus[["pred_config"]] <- pred_config
    return_prediction_object <- add_model_prediction(return_prediction_object, model_function, model_config_plus, back_test)
    }
  }

  # end parallel processing carefully
  future::plan("sequential")
  #closeAllConnections()

  # return data
  return(return_prediction_object)
}
