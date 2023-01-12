#' returns (l(n)- l(n-k))_n
#'
#' @param l
#' @param k
#'
#' @return
#' @export
#'
#' @examples
delta <- function(l,k){
  l-xprev(l,k)
}
