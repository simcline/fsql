#' coalesce fills missing values
#' @param table a dataframe
#' @param value a value
#' @return dataframe whose missing values have been replaced by value
#' @export
coalesce <- function(table, value) {
  table[is.na(table)] <- value
  table
}

#' Infix version of coalesce
#'
#' @param x
#' @param y
#'
#' @return coalesce(y,x)
#' @export
#'
#' @examples
`%^%` <- function(x, y) {
  coalesce(y, x)
}
