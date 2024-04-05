#' Portfolio performance evaluation
#'
#' @param x vector of numbers
#'
#' @return numbers
#'
#' @importFrom stats na.omit
#'
#' @export
#'
#' @examples
#' performance(c(1,2,3,NA,5))
performance <- function(x) {
  mean(stats::na.omit(x))
}
