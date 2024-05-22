#' @Title Backtesting Function for Stock Predictions
#' This function performs backtesting on stock data to evaluate the performance of a predictive model.
#' It splits the data into in-sample and out-of-sample periods and applies a specified weighting function to generate portfolio results.
#' @param data A dataframe containing the stock data, including features and dates
#' @param ticks A vector of feature names used for model training and prediction
#' @param t_oos A date used to separate the in-sample and out-of-sample periods. The format should be "YYYY-MM-DD"
#' @param m_offset An integer representing the offset for the moving window approach
#' @param train_size An integer specifying the size of the training set
#' @param weight_func A function that computes the weights for the portfolio
#'
#' @return A list of results from the backtesting process, with each element corresponding to an out-of-sample period
#' @export
#'
#' @examples xy
backtesting <- function(data, features,sep_date, m_offset, train_size, weight_func){
  ticks <- data$stock_id %>%                               # List of all asset ids
    as.factor() %>%
    levels()
  t_oos <- data$date[data$date > as.Date(sep_date)] %>%           # Out-of-sample dates
    unique() %>%                                                          # Remove duplicates
    as.Date(origin = "1970-01-01")
  results <- purrr::map(1:length(t_oos), portf_map, data = data, features = features,ticks = ticks,
                        t_oos = t_oos, m_offset = m_offset, train_size = train_size,
                        weight_func = weight_func)
  return(results)
}
#'
#' @param data xy
#' @param ticks xy
#' @param t_oos xy
#' @param m_offset xy
#' @param train_size xy
#' @param weight_func xy
#'
#' @return xy
#' @export
#'
#' @examples xy
backtesting <- function(data, features,sep_date, m_offset, train_size, weight_func){
  ticks <- data$stock_id %>%                               # List of all asset ids
    as.factor() %>%
    levels()
  t_oos <- data$date[data$date > as.Date(sep_date)] %>%           # Out-of-sample dates
    unique() %>%                                                          # Remove duplicates
    as.Date(origin = "1970-01-01")
  results <- purrr::map(1:length(t_oos), portf_map, data = data, features = features,ticks = ticks,
                 t_oos = t_oos, m_offset = m_offset, train_size = train_size,
                 weight_func = weight_func)
  return(results)
}

