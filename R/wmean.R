
#' quick weighted mean with na.rm=T
#'
#' @param x vector
#' @param w weight vector
#' @param ... other params passed to mean
#'
#' @return
#' @export
#'
#' @examples
#' 
#'  x<- c(NA,2,3,4)
#'  w <- c(1,NA,2,2)
#'  wmean(x,w)
wmean <- function(x,w=NULL,...){
  if (is.null(w)){
    mean(x, na.rm=T,...)
  } else {
    wDen <- ifelse(is.na(x), NA, w)
    mean(x*w, na.rm=T, ...)/mean(wDen, na.rm=T, ...)
  }
}