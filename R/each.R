#' each
#'
#' @param l list
#' @param fun function
#' @param ungroup boolean wether to ungroup result or not
#' @export
#'
each <- function(l, fun, ungroup=FALSE) {
  if(is.data.frame(l)) {
    l <- split(l, 1:nrow(l))
  }
  m <- lapply(l, fun)
  if(!ungroup) {
    return(m)
  } else {
    return(m %>% ungroup)
  }
}

