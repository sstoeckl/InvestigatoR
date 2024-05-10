library(dplyr)
library(ggplot2)

back_test <- backtesting(data = data_ml, sep_date = "2014-01-01", m_offset = 1, train_size = 5, weight_func = weights_xgb)

# Assuming back_test is your list
returns <- sapply(back_test, function(x) x$returns)
# Total Return
total_return <- sum(returns)

# Average Return
average_return <- mean(returns)

# Standard Deviation of Returns
std_dev_returns <- sd(returns)

# Assuming a risk-free rate (example: 0.5% annualized, adjust according to your frequency)
risk_free_rate <- 0.005
annualized_rf_rate <- risk_free_rate / length(returns)  # Adjust for the period

# Sharpe Ratio
sharpe_ratio <- (mean(returns) - annualized_rf_rate) / sd(returns)

# Maximum Drawdown
max_drawdown <- function(returns) {
  cumulative_returns <- cumprod(1 + returns)
  drawdowns <- 1 - cumulative_returns / cummax(cumulative_returns)
  max(drawdowns)
}
md <- max_drawdown(returns)
cat("Total Return:", total_return, "\n",
    "Average Return:", average_return, "\n",
    "Standard Deviation of Returns:", std_dev_returns, "\n",
    "Sharpe Ratio:", sharpe_ratio, "\n",
    "Maximum Drawdown:", md, "\n")

# Calculate cumulative returns
cumulative_returns <- cumprod(1 + returns)


# Create a data frame for plotting
equity_df <- data.frame(
  Period = 1:length(cumulative_returns),
  CumulativeReturns = cumulative_returns
)

# Plot the equity curve
ggplot(equity_df, aes(x = Period, y = CumulativeReturns)) +
  geom_line(color = "blue") +
  labs(title = "Equity Curve",
       x = "Period",
       y = "Cumulative Returns") +
  theme_minimal()

