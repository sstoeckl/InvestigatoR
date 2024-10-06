# # tests/testthat/helper-create_return_prediction.R
#
# library(testthat)
# library(InvestigatoR)
# library(tidyverse)
#
# # Load the data_ml dataset
# data("data_ml")
#
# # Create a small subset of data_ml for testing
# test_data_ml <- data_ml %>%
#   filter(stock_id<=5)
#
# # Define common parameters for testing
# return_label <- "R1M_Usd"
# features <- c("Div_Yld", "Eps", "Mkt_Cap_12M_Usd", "Mom_11M_Usd", "Ocf", "Pb", "Vol1Y_Usd")
# rolling <- FALSE
# window_size <- "10 years"
# step_size <- "10 month"
# offset <- "1 month"
# in_sample <- TRUE
#
# # Define ML configurations for testing
# ml_config_mock <- list(
#   ols_pred = list(
#     pred_func = "ols_pred",
#     config = list()
#   ),
#   enet_pred = list(
#     pred_func = "enet_pred",
#     config1 = list(alpha = 0.5, lambda = 0.1),
#     config2 = list(alpha = 0.5, lambda = 0.5)
#   ),
#   xgb_pred = list(
#     pred_func = "xgb_pred",
#     config1 = list(nrounds = 1, max_depth = 3, eta = 0.3, objective = "reg:squarederror"),
#     config2 = list(nrounds = 1, max_depth = 3, eta = 0.1, objective = "reg:squarederror")
#   ),
#   rf_pred = list(
#     pred_func = "rf_pred",
#     config1 = list(num.trees = 10, mtry = 4),
#     config2 = list(num.trees = 5, mtry = 3)
#   )
# )
# # tests/testthat/test-ensemble_prediction.R
# # Run backtesting_returns with all models
# rp_mock <- backtesting_returns(
#   data = test_data_ml,
#   return_prediction_object = NULL,
#   return_label = return_label,
#   features = features,
#   rolling = rolling,
#   window_size = window_size,
#   step_size = step_size,
#   offset = offset,
#   in_sample = in_sample,
#   ml_config = ml_config_mock,
#   append = FALSE,
#   num_cores = 1L,  # Use single core for testing
#   verbose = TRUE  # Set to TRUE for detailed messages during testing
# )
#
# test_that("ensemble_prediction computes simple average correctly", {
#   # Define ensemble configuration for simple average
#   config_simple <- list(
#     method = "simple_average"
#   )
#
#   # Apply ensemble prediction
#   ensemble_pred_simple <- ensemble_prediction(rp_mock, config_simple, new_column = "ensemble_pred_simple")
#
#   # Manually calculate expected simple average
#   expected <- rp_mock$predictions %>%
#     rowwise() |>
#     dplyr::mutate(pred_return = (ols_1+enet_1+enet_2+rf_1+rf_2+xgb_1+xgb_2)/7) %>%
#     dplyr::select(stock_id, date, pred_return) |>  ungroup()
#
#   # Compare
#   expect_equal(round(ensemble_pred_simple$pred_return, 5), round(expected$pred_return, 5))
# })
#
# test_that("ensemble_prediction computes weighted average correctly with provided weights", {
#   # Define ensemble configuration for weighted average
#   config_weighted <- list(
#     method = "weighted_average",
#     weights = c(0.5, 0.1, 0.1, 0.05, 0.05, 0.1, 0.1)  # Sum to 1
#   )
#
#   # Apply ensemble prediction
#   ensemble_pred_weighted <- ensemble_prediction(rp_mock, config_weighted, new_column = "ensemble_pred_weighted")
#
#   # Manually calculate expected weighted average
#   weights <- c(0.5, 0.1, 0.1, 0.05, 0.05, 0.1, 0.1)
#   expected <- rp_mock$predictions %>%
#     mutate(pred_return = (ols_1 * weights[1]) + (enet_1 * weights[2]) +
#              (enet_2 * weights[3])
#            + (xgb_1 * weights[4]) + (xgb_2 * weights[5])
#            + (rf_1 * weights[6]) + (rf_2 * weights[7])
#            ) %>%
#     select(stock_id, date, pred_return)
#
#   # Compare
#   expect_equal(round(ensemble_pred_weighted$pred_return, 5), round(expected$pred_return, 5))
# })
#
# test_that("ensemble_prediction computes weighted average correctly without provided weights", {
#   # Define ensemble configuration for weighted average without weights
#   config_weighted_auto <- list(
#     method = "weighted_average"
#   )
#
#   # Apply ensemble prediction
#   ensemble_pred_weighted_auto <- ensemble_prediction(rp_mock, config_weighted_auto, new_column = "ensemble_pred_weighted_auto")
#
#   # Calculate inverse of average errors as weights
#   avg_errors <- rp_mock$errors %>%
#     summarise(across(3:last_col(), mean, na.rm = TRUE)) %>%
#     unlist()
#   weights <- 1 / avg_errors
#   weights <- weights / sum(weights)
#
#   # Manually calculate expected weighted average
#   expected <- rp_mock$predictions %>%
#     mutate(pred_return_weighted_auto = (ols_1 * weights["ols_1"]) +
#              (enet_1 * weights["enet_1"]) + (enet_2 * weights["enet_2"]) +
#              (xgb_1 * weights["xgb_1"]) + (xgb_2 * weights["xgb_2"]) +
#              (rf_1 * weights["rf_1"]) + (rf_2 * weights["rf_2"])
#            ) %>%
#     select(stock_id, date, pred_return_weighted_auto) %>%
#     rename(pred_return = pred_return_weighted_auto)
#
#   # Compare
#   expect_equal(round(ensemble_pred_weighted_auto$pred_return, 5), round(expected$pred_return, 5))
# })
#
# test_that("ensemble_prediction computes minimum variance weights correctly (unconstrained)", {
#   # Define ensemble configuration for minimum variance unconstrained
#   config_minvar_unconstrained <- list(
#     method = "min_var_unconstrained"
#   )
#
#   # Apply ensemble prediction
#   ensemble_pred_minvar_unconstrained <- ensemble_prediction(rp_mock, config_minvar_unconstrained, new_column = "ensemble_pred_minvar_unconstrained")
#
#   # Manually compute expected weights using quadprog
#   errors_df <- rp_mock$errors %>%
#     dplyr::select(ends_with("_pred"))
#
#   cov_matrix <- cov(errors_df, use = "pairwise.complete.obs")
#
#   Dmat <- 2 * cov_matrix
#   dvec <- rep(0, ncol(cov_matrix))
#   Amat <- matrix(1, nrow = ncol(cov_matrix), ncol = 1)
#   bvec <- 1
#   meq <- 1
#
#   qp_solution <- quadprog::solve.QP(Dmat, dvec, Amat, bvec, meq = meq)
#   weights <- qp_solution$solution
#
#   # Manually calculate ensemble predictions
#   predictions_matrix <- as.matrix(rp_mock$predictions %>% select(all_of(prediction_cols)))
#   expected <- tibble::tibble(
#     stock_id = rp_mock$predictions$stock_id,
#     date = rp_mock$predictions$date,
#     pred_return = predictions_matrix %*% weights
#   )
#
#   # Compare
#   expect_equal(round(ensemble_pred_minvar_unconstrained$pred_return, 5), round(as.numeric(expected$pred_return), 5))
# })
#
# test_that("ensemble_prediction computes minimum variance weights correctly (constrained)", {
#   # Define ensemble configuration for minimum variance constrained
#   config_minvar_constrained <- list(
#     method = "min_var_constrained"
#   )
#
#   # Apply ensemble prediction
#   ensemble_pred_minvar_constrained <- ensemble_prediction(rp_mock, config_minvar_constrained, new_column = "ensemble_pred_minvar_constrained")
#
#   # Manually compute expected weights using quadprog with constraints
#   errors_df <- rp_mock$errors %>%
#     dplyr::select(ends_with("_pred"))
#
#   cov_matrix <- cov(errors_df, use = "pairwise.complete.obs")
#
#   Dmat <- 2 * cov_matrix
#   dvec <- rep(0, ncol(cov_matrix))
#   Amat <- cbind(rep(1, ncol(cov_matrix)), diag(ncol(cov_matrix)))
#   bvec <- c(1, rep(0, ncol(cov_matrix)))
#   meq <- 1
#
#   qp_solution <- quadprog::solve.QP(Dmat, dvec, Amat, bvec, meq = meq)
#   weights <- qp_solution$solution
#
#   # Manually calculate ensemble predictions
#   predictions_matrix <- as.matrix(rp_mock$predictions %>% select(all_of(prediction_cols)))
#   expected <- tibble::tibble(
#     stock_id = rp_mock$predictions$stock_id,
#     date = rp_mock$predictions$date,
#     pred_return = predictions_matrix %*% weights
#   )
#
#   # Compare
#   expect_equal(round(ensemble_pred_minvar_constrained$pred_return, 5), round(as.numeric(expected$pred_return), 5))
# })
