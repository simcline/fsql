#' dict creates a named vector as a python like dictionary
#' @param k vector of character
#' @param v vector of values
#' @return named vector
#' @export
dict <- function(k, v) {
  # create a named list from key/value pair
  # k: a list of keys
  # v: a list of values
  d <- v
  names(d) <- k
  d
}

`%!%` <- function(x, y) {
  dict(x, y)
}
