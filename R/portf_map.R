#' Title
#'
#' @param t xy
#' @param data xyx
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
portf_map <- function(t, data, ticks, t_oos, m_offset, train_size, weight_func){
  train_data <- data %>% filter(date < t_oos[t] - m_offset * 30,   # Rolling window with buffer
                                date > t_oos[t] - m_offset * 30 - 365 * train_size)
  test_data <- data %>% filter(date == t_oos[t])                   # Test set
  realized_returns <- test_data %>%
    dplyr::select(R1M_Usd)                                          # 1M holding period
  temp_weights <- weight_func(train_data = train_data, test_data = test_data)      # Weights calculation
  ind <- match(temp_weights$names, ticks) %>% na.omit()             # Index of test assets
  x <- list(weights = rep(0, length(ticks)))                        # Initialize weights
  x$weights[ind] <- temp_weights$weights                            # Set weights
  x$returns <- sum(temp_weights$weights * realized_returns$R1M_Usd) # Compute returns
  return(x)
}
