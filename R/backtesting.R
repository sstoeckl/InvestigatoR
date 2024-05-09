#' Title
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
backtesting <- function(data, sep_date, m_offset, train_size, weight_func){
  ticks <- data$stock_id %>%                               # List of all asset ids
    as.factor() %>%
    levels()
  t_oos <- data$date[data$date > as.Date(sep_date)] %>%           # Out-of-sample dates
    unique() %>%                                                          # Remove duplicates
    as.Date(origin = "1970-01-01")
  results <- purrr::map(1:length(t_oos), portf_map, data = data, ticks = ticks,
                 t_oos = t_oos, m_offset = m_offset, train_size = train_size,
                 weight_func = weight_func)
  return(results)
}
