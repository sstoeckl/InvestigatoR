predict_stock <- function(predictions) {
    # Apply the transformation across the matrix
    holdings_matrix <- apply(prediction_matrix, c(1, 2), function(x) {
      if (x > 0) {
        return(1)  # Buy if the prediction is positive
      } else if (x < 0) {
        return(-1)  # Sell if the prediction is negative
      } else {
        return(0)  # No action if the prediction is zero
      }
    })
    return(holdings_matrix)
  }
