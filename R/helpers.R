#' Helper function to convert any time period to days
#'
#' @param period A string that contains a time period, e.g. "3 days", "1 week", "2 months", "1 year"
#'
#' @return The number of days in the time period
#'
#' @export
#' @examples
#' convert_period_to_days("3 days")
#' convert_period_to_days("1 week")
#' convert_period_to_days("2 months")
#' convert_period_to_days("1 year")
convert_period_to_days <- function(period) {
  num <- as.numeric(gsub("[^0-9]", "", period))  # Extract the number
  unit <- gsub(" ","",gsub("[0-9\\s]", "", period))  # Extract the text part

  switch(tolower(unit),
         "day" = num,
         "days" = num,
         "week" = num * 7,
         "weeks" = num * 7,
         "month" = num * 30,  # Approximate
         "months" = num * 30,  # Approximate
         "quarter" = num * 91,  # Approximate
         "quarters" = num * 91,  # Approximate
         "year" = num * 365,  # Approximate
         "years" = num * 365,  # Approximate
         stop("Invalid time unit.")
  )
}

#' Helper function to create a tibble of training/prediction start and end dates based on a rolling window with an offset
#'
#' @param dates A vector of unique dates in increasing order
#' @param window_size A string that contains the window size, e.g. "3 days", "1 week", "2 months", "1 year"
#' @param step_size A string that contains the step size, e.g. "3 days", "1 week", "2 months", "1 year"
#' @param offset A string that contains the offset, e.g. "3 days", "1 week", "2 months", "1 year"
#' @param rolling A boolean that indicates whether the window should be rolling or expanding (FALSE)
#'
#' @return A tibble with columns training_start, training_end, prediction_start, prediction_end, and prediction_phase
#'
#' @importFrom lubridate days
#' @importFrom dplyr filter mutate lag
#' @importFrom tidyr unnest
#'
#' @export
#'
#' @examples
#' dates <- seq(as.Date("2020-01-01"), as.Date("2023-01-01"), by = "month")
#' window_size <- "1 year"
#' step_size <- "3 months"
#' offset <- "1 month"
#' date_intervals <- select_dates_by_offset(dates, window_size, step_size, offset)
#' print(date_intervals)
#' date_intervals_exp <- select_dates_by_offset(dates, window_size, step_size, offset, rolling=FALSE)
#' print(date_intervals_exp)
#'
select_dates_by_offset <- function(dates, window_size, step_size, offset, rolling=TRUE) {
  dates <- as.Date(dates)

  # Convert window_size, step_size, and offset to days
  window_days <- convert_period_to_days(window_size)
  step_days <- convert_period_to_days(step_size)
  offset_days <- convert_period_to_days(offset)

  # Calculate start date for rolling based on the offset and window size
  start_date <- min(dates)
  end_date <- max(dates) - window_days - offset_days - step_days

  # Initialize the date list and first date for rolling
  target_dates <- c(start_date)
  current_date <- start_date

  # Populate target dates taking into account the step_days and not exceeding the end_date
  while ((current_date + lubridate::days(step_days)) <= end_date + lubridate::days(offset_days) + lubridate::days(step_days)) {
    current_date <- current_date + lubridate::days(step_days)
    target_dates <- c(target_dates, current_date)
  }

  # Generate a tibble with training and prediction windows
  date_intervals <- tibble(
    training_start = target_dates,
    training_end = target_dates + lubridate::days(window_days) - lubridate::days(1),
    prediction_start = target_dates + lubridate::days(window_days) + lubridate::days(offset_days),
    prediction_end = target_dates + lubridate::days(window_days) + lubridate::days(offset_days) + lubridate::days(step_days) - lubridate::days(1),
    prediction_phase = "OOS"
  )

  # Map calculated dates back to the nearest real dates available in the dataset
  date_intervals <- date_intervals %>%
    dplyr::mutate(training_start = map(training_start, ~ dates[which.min(abs(dates - .x))]),
           training_end = map(training_end, ~ dates[which.min(abs(dates - .x))]),
           prediction_start = map(prediction_start, ~ dates[which.min(abs(dates - .x))]),
           prediction_end = map(prediction_end, ~ dates[which.min(abs(dates - .x))]))
  if (!rolling) {
    date_intervals <- date_intervals %>%
      mutate(training_start = start_date)
  }
  date_intervals <- date_intervals |> tidyr::unnest(everything())

  # consistency check
  #remove rows with zero preditions
  date_intervals <- date_intervals |> dplyr::filter(prediction_start!=prediction_end)
  # adjust last row to have prediction end date beyond dataset
  date_intervals$prediction_end[nrow(date_intervals)] <- max(dates)+lubridate::days(step_days)
  # check that always prediction_start = prediction end
  date_intervals <- date_intervals |>
    dplyr::mutate(prediction_start=as.Date(ifelse(!is.na(dplyr::lag(prediction_end))&prediction_start>=dplyr::lag(prediction_end),dplyr::lag(prediction_end),prediction_start)))

  return(date_intervals)
}

#' Helper function to create performance metrics for a given set of returns, weights, and actual data
#'
#' @param returns_data return data
#' @param weights_data weights data
#' @param actual_data actual realized returns
#'
#' @importFrom dplyr group_by summarise left_join mutate
#' @importFrom tidyr pivot_longer
#'
#' @return A tibble with columns portfolio, mean, sd, SR, VaR_5, turnover, and hit_ratio
#' @export
#'
perf_met <- function(returns_data, weights_data, actual_data){

  stats <- returns_data |>
    tidyr::pivot_longer(cols = -date, names_to = "portfolio", values_to = "return") |>
    dplyr::group_by(portfolio) |>
    dplyr::summarise(mean=mean(return),sd=sd(return),SR=mean/sd,VaR_5=quantile(return,0.05), ,.groups="drop")

  turnover <- weights_data |>
    tidyr::pivot_longer(cols = 3:last_col(), names_to = "portfolio", values_to = "weight") |>
    dplyr::left_join(actual_data, by = c("stock_id","date")) |>
    dplyr::group_by(stock_id,portfolio) |>
    dplyr::mutate(prior_weight=dplyr::lag(weight,default = 0)*(1+actual_return)) |>
    dplyr::group_by(portfolio,date) |>
    dplyr::mutate(turnover=abs(weight-prior_weight)/sum(prior_weight)) |>
    dplyr::group_by(portfolio) |>
    dplyr::summarise(turnover=sum(turnover),.groups="drop")

  stats <- stats |>
    dplyr::left_join(turnover, by = "portfolio")

  return(stats)
}
#' Helper function to find largest number after a certain string
#'
#' @param strings vector of strings to search for
#' @param model_name string to search for the largest number after
#'
#' @return The largest number found after the model_name in the strings
#' @export
#'
find_largest_number <- function(strings, model_name) {
  # Regex to filter strings containing the model_name and extract the last number
  pattern <- paste0(".*", model_name, "_([0-9]+)$")

  # Filter and extract numbers
  numbers <- sapply(strings, function(x) {
    if(grepl(model_name, x)) {
      matches <- regmatches(x, regexpr(pattern, x))
      if(length(matches) > 0) {
        as.numeric(sub(pattern, "\\1", matches))
      } else {
        0
      }
    } else {
      0
    }
  })

  # Remove NA values and find the maximum number
  max_number <- max(numbers, na.rm = TRUE)
  return(max_number)
}
library(dplyr)
library(caret)

#' Encode Categorical Variables
#'
#' This function checks a data frame for categorical variables and applies one-hot encoding
#' if any are found. It provides a message about the changes.
#'
#' @param data A data frame that might contain categorical variables.
#'
#' @return A list containing the modified data frame and a vector of encoded variables.
#'
#' @importFrom dplyr mutate_all select_if
#' @importFrom caret dummyVars
#'
#' @examples
#' data(iris)
#' result <- encode_categorical(iris)
#' print(result$data)  # Modified data
#' print(result$encoded_vars)  # Names of encoded variables
encode_categorical <- function(data) {
  # Identify categorical variables
  categorical_vars <- sapply(data, is.factor) | sapply(data, is.character)

  if (any(categorical_vars)) {
    cat_var_names <- names(categorical_vars[categorical_vars])
    message("Encoding categorical variables: ", paste(cat_var_names, collapse = ", "))

    # Apply one-hot encoding
    dummies <- caret::dummyVars(~ ., data = data, fullRank = FALSE)
    data_transformed <- predict(dummies, newdata = data)

    # Return the modified data frame and the names of encoded variables
    return(list(data = data_transformed, encoded_vars = cat_var_names))
  } else {
    return(list(data = data, encoded_vars = NULL))
  }
}
#' Check for Missing Values
#'
#' This function examines a dataframe for any missing values and issues a warning if missing values are found,
#' reporting the columns with missing values and the count of missing entries.
#'
#' @param data A dataframe to check for missing values.
#'
#' @return A dataframe unchanged, but with a warning message if missing values are found.
#'
check_missing_values <- function(data) {
  # Check for missing values in each column
  missing_summary <- summarise_all(data, ~ sum(is.na(.)))

  # Find columns with missing values
  missing_cols <- names(missing_summary)[which(missing_summary > 0)]

  if (length(missing_cols) > 0) {
    # Create a warning message with specifics about missing data
    warning_message <- paste("Missing values found in columns:", paste(missing_cols, collapse=", "),
                             ". Total missing per column: ", paste(missing_summary[missing_cols], collapse=", "), ".")
    warning(warning_message)
  }
}
