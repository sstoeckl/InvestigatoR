#' Title
#'
#' @param train_data xyx
#' @param test_data xy
#' @param features xy
#'
#' @return xy
#' @export
#'
#' @examples xy
equal_weights_func <- function(train_data, test_data, features = NULL) {
  # Number of assets in the test dataset
  num_assets <- nrow(test_data)

  # Check if there are any assets to weigh
  if (num_assets > 0) {
    # Assign equal weight to each asset
    weights <- rep(1 / num_assets, num_assets)
  } else {
    # No assets available
    weights <- numeric(0)
  }

  # Return a list with names of assets and their corresponding weights
  return(list(names = test_data$stock_id, weights = weights))
}
