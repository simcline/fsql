#' grep function returning the matching values
#'
#' @param pattern pattern to grep
#' @param x list/vector of strings
#'
#' @return the vector of matching values
#' @export
#'
#' @examples
#'  "aa" %grep% c("a","aa", "aab", "caab")
#'
`%grep%` <- function(pattern,x){
  grep(pattern,x,value=T)
}
