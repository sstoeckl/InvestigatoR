#' Helper function to convert any time period to days
#'
#' @param period A string that contains a time period, e.g. "3 days", "1 week", "2 months", "1 year"
#'
#' @return The number of days in the time period
#'
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
  while ((current_date + days(step_days)) <= end_date + days(offset_days) + days(step_days)) {
    current_date <- current_date + days(step_days)
    target_dates <- c(target_dates, current_date)
  }

  # Generate a tibble with training and prediction windows
  date_intervals <- tibble(
    training_start = target_dates,
    training_end = target_dates + days(window_days) - days(1),
    prediction_start = target_dates + days(window_days) + days(offset_days),
    prediction_end = target_dates + days(window_days) + days(offset_days) + days(step_days) - days(1),
    prediction_phase = "OOS"
  )

  # Map calculated dates back to the nearest real dates available in the dataset
  date_intervals <- date_intervals %>%
    mutate(training_start = map(training_start, ~ dates[which.min(abs(dates - .x))]),
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
  date_intervals <- date_intervals |> filter(prediction_start!=prediction_end)
  # adjust last row to have prediction end date beyond dataset
  date_intervals$prediction_end[nrow(date_intervals)] <- max(dates)+days(step_days)
  # check that always prediction_start = prediction end
  date_intervals <- date_intervals |>
    mutate(prediction_start=as.Date(ifelse(!is.na(lag(prediction_end))&prediction_start>=lag(prediction_end),lag(prediction_end),prediction_start)))

  return(date_intervals)
}

perf_met <- function(returns_data, weights_data, actual_data){

  stats <- returns_data |>
    pivot_longer(cols = -date, names_to = "portfolio", values_to = "return") |>
    group_by(portfolio) |>
    summarise(mean=mean(return),sd=sd(return),SR=mean/sd,VaR_5=quantile(return,0.05), ,.groups="drop")

  turnover <- weights_data |>
    pivot_longer(cols = 3:last_col(), names_to = "portfolio", values_to = "weight") |>
    left_join(actual_data, by = c("stock_id","date")) |>
    group_by(stock_id,portfolio) |>
    mutate(prior_weight=dplyr::lag(weight,default = 0)*(1+actual_return)) |>
    group_by(portfolio,date) |>
    mutate(turnover=abs(weight-prior_weight)/sum(prior_weight)) |>
    group_by(portfolio) |>
    summarise(turnover=sum(turnover),.groups="drop")

  stats <- stats |>
    left_join(turnover, by = "portfolio")

  return(stats)
}


