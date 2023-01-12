#' remove first y values from x
#' @param x a vector
#' @param y an integer
#' @return vector with y values dropped from front of x
#' @export
`%_%` <- function(x, y) {
  tail(y, -x)
}
