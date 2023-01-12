#' like operator similar to the SQL like keyword
#'
#' @param x a list/vector/data.frame
#' @param y a regular expression
#'
#' @return Boolean vector giving the position of matches
#' @export
#'
#' @examples
`%like%` <- function(x, y) {
  grepl(y, x)
}
