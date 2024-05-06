utils::globalVariables(c(
  "stock_id", "date", "open", "high", "low", "close", "volume",
  "adjusted_close", "market_cap", "pe_ratio", "dividend_yield",
  "earnings_per_share", "price_to_book", "beta", "moving_average_50",
  "moving_average_200", "rsi", "macd", "sector", "industry", "exchange"
))


#stock_id: Unique identifier for each stock.
#date: Trading date.
#open, high, low, close: Opening, highest, lowest, and closing prices for the stock on a given day.
#volume: Number of shares traded during the day.
#adjusted_close: Closing price adjusted for dividends and splits.
#market_cap: Total market value of a company's outstanding shares.
#pe_ratio: Price-to-earnings ratio, a measure of the stock price relative to its earnings.
#dividend_yield: Indicates how much a company pays out in dividends each year relative to its stock price.
#earnings_per_share: Profit allocated to each outstanding share of common stock.
#price_to_book: Ratio of market value to book value per share.
#beta: Measure of the volatility—or systematic risk—of a security or portfolio compared to the market as a whole.
#moving_average_50, moving_average_200: The 50-day and 200-day moving averages.
#rsi: Relative Strength Index, a momentum oscillator that measures the speed and change of price movements.
#macd: Moving Average Convergence Divergence, a trend-following momentum indicator.
#sector, industry: Industry sector and specific industry category the stock belongs to.
#exchange: The stock exchange on which the stock is traded.
