#' Pick first element
#'
#' @param x any structure compatible with head
#'
#' @return first element
#' @export
#'
#' @examples
#' x <- c(1,2,3)
#' first(x)
first <- function(x) {
  head(x,1)
}

#' Pick last element
#'
#' @param x any structure compatible with head
#'
#' @return last element
#' @export
#'
#' @examples
#' x <- c(1,2,3)
#' last(x)
last <- function(x) {
  tail(x,1)
}

#' Pick nth element
#'
#' @param x any structure compatible with simple-bracket slicing
#' @param n index
#'
#' @return element in position n of x
#' @export
#'
#' @examples
#' x <- c(1,2,3)
#' nth(x,2)
nth <- function(x,n) {
  x[n]
}
