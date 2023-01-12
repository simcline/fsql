#' SQL-like ascending order. Sort is by the first column given, then by the second column within the first, and so on.
#'
#' @param d data.frame
#' @param cols char vector of column names
#'
#' @return sorted data.frame
#' @export
#'
#' @examples
#' d <- data.frame(
#'date=seq(as.Date("2001-01-01"), as.Date("2001-10-01"), by="month"),
#'ticker=sample(c("1 HK", "2 HK"), 10, TRUE),
#'ret1=rnorm(10),
#'ret2=rnorm(10),
#'ret3=rnorm(10)
#')
#'
#'d %>% xasc("ret1")
xasc <- function(d, cols) {
  s <- paste("with(d, order(",paste(cols,collapse=","),"))",sep="")
  d[eval(parse(text=s)), ]

}

