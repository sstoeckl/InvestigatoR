#' Title
#'
#' @param data xy
#' @param labels xy
#' @param features xy
#' @param start_date xy
#' @param end_date xy
#' @param seperation_date xy
#'
#' @return xy
#' @export
#'
#' @examples xy
data_seperation <- function(data, labels, features, start_date, end_date, seperation_date) {
  # Convert time series data to a data frame if necessary
  if (inherits(data, "ts") || inherits(data, "mts")) {
    data <- data.frame(date = as.Date(time(data), origin = "1970-01-01"), data, check.names = FALSE)
  }

  # Ensure 'stock_id' column exists
  if (!("stock_id" %in% names(data))) {
    stop("The 'data' dataframe does not contain the 'stock_id' column.")
  }

  # Filter and prepare the data
  data <- data %>%
    dplyr::filter(date > as.Date(start_date), date < as.Date(end_date)) %>%
    dplyr::arrange(stock_id, date)

  # Identify stocks with complete data across the timeframe
  stock_ids <- unique(data$stock_id)
  stock_days <- data %>%
    dplyr::group_by(stock_id) %>%
    dplyr::summarize(nb = dplyr::n(), .groups = 'drop')
  full_data_stocks <- stock_ids[stock_days$nb == max(stock_days$nb)]

  # Filter the dataset to include only stocks with complete data
  data <- data %>%
    dplyr::filter(stock_id %in% full_data_stocks)

  # Define features and labels
  features <- names(data)[4:ncol(data)]  # Adjust index according to your dataset

  # Split the data
  separation_date <- as.Date(seperation_date)
  training_sample <- data %>% dplyr::filter(date < seperation_date)
  testing_sample <- data %>% dplyr::filter(date >= seperation_date)

  # Prepare train and test datasets
  train_features <- training_sample %>% dplyr::select(dplyr::all_of(features))
  train_labels <- training_sample[[labels]]
  test_features <- testing_sample %>% dplyr::select(dplyr::all_of(features))
  test_labels <- testing_sample[[labels]]

  return(list(
    train_features = train_features,
    train_labels = train_labels,
    test_features = test_features,
    test_labels = test_labels
  ))
}

