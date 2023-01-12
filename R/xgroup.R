#' Split a data.frame into a list of data.frame by values of a subset of columns
#'
#' @param d data.frame
#' @param cols character vector of columns with respect to which perform the group operation.
#'
#' @return a list of sub-blocks data.frames following the grouping rule
#' @export
#'
#' @examples
#'
#'d <- data.frame(
#'date=seq(as.Date("2001-01-01"), as.Date("2001-10-01"), by="month"),
#'ticker=sample(c("1 HK", "2 HK"), 10, TRUE),
#'ret1=rnorm(10),
#'ret2=rnorm(10),
#'ret3=rnorm(10)
#')
#'
#'d %>% xgroup("ticker")
xgroup <- function(d, cols=NULL) {
  key <- paste(paste("(",paste(cols,collapse=","), ")", sep=""),
                 "!", paste("(", apply(d[, cols,FALSE], 1, paste, collapse=","), ")", sep=""))
  by(d, key, function(x) x, simplify = F)
}
