#' Return the vector with an offset of k indices
#'
#' @param l list/vector
#' @param k offset
#'
#' @return offset list/vector
#' @export
#'
#' @examples
#'
#'x <- c(1,3,5)
#'xprev(x,1)
#'xprev(x,-1)
xprev <- function(l, k) {
  if(k==0) {
    return(l)
  }
  if(k>=0) {
    return(c(rep(NA,min(k, length(l))), head(l, -min(k, length(l)))))
  } else {
    return(c(tail(l, max(k, -length(l))), rep(NA, min(-k, length(l)))))
  }
}
