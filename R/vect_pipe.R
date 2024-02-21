#' Vectorized pipe
#'
#' @param x
#' @param f
#'
#' @return a vector containing f(x_i) for all i
#' @export
#'
#' @examples
#' c(1,2,3) %.>% (\(x) x+1)
`%.>%` <- function(x,f){
  x %>% each(f) %>% unlist
}
