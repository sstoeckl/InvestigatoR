# tests/testthat/helper-data.R

# Sample data_ml subset
data_ml_subset <- tibble::tibble(
  stock_id = rep(1:3, each = 4),
  date = rep(seq.Date(from = as.Date("2020-01-01"), by = "month", length.out = 4), times = 3),
  R1M_Usd = c(0.05, 0.02, -0.03, 0.04, 0.01, -0.02, 0.03, 0.05, -0.01, 0.02, 0.04, 0.03),
  Div_Yld = runif(12, 1, 5),
  Eps = runif(12, 0.5, 2),
  Mkt_Cap_12M_Usd = runif(12, 100, 500),
  Mom_11M_Usd = runif(12, -0.1, 0.1),
  Ocf = runif(12, 10, 50),
  Pb = runif(12, 0.5, 3),
  Vol1Y_Usd = runif(12, 1000, 5000)
)
