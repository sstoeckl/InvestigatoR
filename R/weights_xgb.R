#' Title
#'
#' @param train_data xy
#' @param test_data xy
#' @param features xy
#'
#' @return xy
#' @export
#'
#' @examples xy
weights_xgb <- function(train_data, test_data, features){
  train_features <- train_data %>% dplyr::select(features) %>% as.matrix()  # Indep. variable
  train_label <- train_data$R12M_Usd / exp(train_data$Vol1Y_Usd)            # Dep. variable
  ind <- which(train_label < quantile(train_label,0.2)|                     # Filter
                 train_label > quantile(train_label, 0.8))
  train_features <- train_features[ind, ]                                   # Filt'd features
  train_label <- train_label[ind]                                           # Filtered label
  train_matrix <- xgb.DMatrix(data = train_features, label = train_label)   # XGB format
  fit <- train_matrix %>%
    xgb.train(data = .,                       # Data source (pipe input)
              eta = 0.3,                      # Learning rate
              objective = "reg:squarederror", # Number of random trees
              max_depth = 4,                  # Maximum depth of trees
              nrounds = 80,                   # Number of trees used
              verbose = 0                     # No comments
    )
  xgb_test <- test_data %>%                     # Test sample => XGB format
    dplyr::select(features) %>%
    as.matrix() %>%
    xgb.DMatrix()

  pred <- predict(fit, xgb_test)                # Single prediction
  w <- pred > median(pred)                      # Keep only the 50% best predictions
  w$weights <- w / sum(w)
  w$names <- unique(test_data$stock_id)
  return(w)                                     # Best predictions, equally-weighted
}
