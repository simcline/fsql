
#' quick weighted sd with na.rm=T
#'
#' @param x vector
#' @param w weight vector
#' @param ... other params passed to sd/mean
#'
#' @return
#' @export
#'
#' @examples
#'
#'  x<- c(NA,2,3,4)
#'  w <- c(1,NA,2,2)
#'  wsd(x,w)
wsd <- function(x,w=NULL,...){
  if (is.null(w)){
    sd(x, na.rm=T,...)
  } else {
    wDen <- ifelse(is.na(x), NA, w)
    wm <- mean(x*w, na.rm=T, ...)/mean(wDen, na.rm=T, ...)
    sqrt(mean((x- wm)^2*w, na.rm=T, ...)/mean(wDen, na.rm=T, ...))
  }
}
