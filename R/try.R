#' Try to compute an expression and returns default value if an error is raised
#'
#' @param x the default value
#' @param y the expression to evaluate
#'
#' @return the value to compute
#' @export
#'
#' @examples
#'
#' NA %try% (5+5)
`%try%` <- function(x,y){
  y <- unparse(substitute(y))
  tryCatch(eval_char(y), error=\(e) x)
}
