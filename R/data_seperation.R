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
# Filter and prepare the data
data <- data %>%
  filter(date > start_date, date < end_date) %>%
  arrange(stock_id, date)

# Identify stocks with complete data across the timeframe
stock_ids <- unique(data_ml$stock_id)
stock_days <- data %>% group_by(stock_id) %>% summarize(nb = n())
full_data_stocks <- stock_ids[stock_days$nb == max(stock_days$nb)]

# Filter the dataset to include only stocks with complete data
data <- data %>% filter(stock_id %in% full_data_stocks)

# Define features and labels
features <- names(data)[4:ncol(data)]  # Adjust index according to your dataset

# Select a separation date to split the data into training and testing
separation_date <- as.Date(seperation_date)

# Split the data
training_sample <- data %>% filter(date < separation_date)
testing_sample <- data %>% filter(date >= separation_date)

# Prepare train and test datasets
train_features <- training_sample %>% select(all_of(features))  # Select only feature columns for training
train_labels <- training_sample[[labels]]  # Select labels for training

test_features <- testing_sample %>% select(all_of(features))  # Select only feature columns for testing
test_labels <- testing_sample[[labels]]  # Select labels for testing

return(train_features, train_labels, test_features, test_labels, labels, features)
}
