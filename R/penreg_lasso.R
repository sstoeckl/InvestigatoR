#change the name to penreg without lasso
#' Title
#'
#' @param y_penalized xy
#' @param x_penalized xy
#'
#' @return xy
#' @export
#'
#' @examples xy

penreg <- function(y_penalized, x_penalized) {

for(i in seq(0,1,by = 0.1)){
fit_penreg <- glmnet(x_penalized, y_penalized, alpha)
  }
return(fit_lasso, fit_ridge)
}
