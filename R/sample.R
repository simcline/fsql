#' Uniform sampling with replacement
#'
#' @param x sampling size
#' @param y vector/list
#'
#' @return subsampled  vector/list
#' @export
#'
#' @examples
#' 5 %?% c(1,3,4)
`%?%` <- function(x, y) {
  sample(y, x, replace=TRUE)
}
