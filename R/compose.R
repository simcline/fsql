#' function composition
#' @param f a function
#' @param g a function
#' @return function
#' @export
`%o%` <- function(f, g) {
  Compose(f, g)
}
