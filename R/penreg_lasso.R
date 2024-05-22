#'
#' @Title Penalized Regression without Lasso
#' This function performs penalized regression using glmnet with a sequence of alpha values ranging from 0 to 1
#' @param y_penalized A vector containing the dependent variable
#' @param x_penalized A matrix or dataframe containing the independent variables
#'
#' @return A list containing the fitted models for different alpha values
#' @export
#'
#' @examples
#'
#' penreg <- function(y_penalized, x_penalized) {
#'
#' for(i in seq(0,1,by = 0.1)){
#' fit_penreg <- glmnet(x_penalized, y_penalized, alpha)
#'   }
#' return(fit_lasso, fit_ridge)
#' }
