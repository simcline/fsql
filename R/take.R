#' Infix shortcut for head
#'
#' @param x number of elements to pull out
#' @param y vector/list/data.frame...
#'
#' @return head(y,x)
#' @export
#'
#' @examples
#' 5 %#% 1:25
`%#%` <- function(x, y) {
  head(y, x)
}
