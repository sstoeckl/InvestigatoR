
data_res <- data_seperation(data = data_ml, labels = "R1M_Usd", features = features_short, start_date = "1999-12-31", end_date = "2019-01-01", seperation_date = "2014-01-15")

train_features <- data_res$train_features
train_labels <- data_res$train_labels
test_features <- data_res$test_features
test_labels <- data_res$test_labels


t_oos <- data_ml$date[data_ml$date > as.Date("2007-01-01")] %>%           # Out-of-sample dates
  unique() %>%                                                          # Remove duplicates
  as.Date(origin = "1970-01-01")

backtesting(data = data_ml, t_oos = t_oos, m_offset = 1, train_size = 5, weight_func = equal_weights_func())
