#' Sets the primary keys in a table.
#'
#' @param d data.frame
#' @param keys char vector listing the key columns
#'
#' @return data.frame with an additional key attribute
#' @export
#'
#' @examples
#'
#' d <- data.frame(
#'date=seq(as.Date("2001-01-01"), as.Date("2001-10-01"), by="month"),
#'ticker=sample(c("1 HK", "2 HK"), 10, TRUE),
#'ret1=rnorm(10),
#'ret2=rnorm(10),
#'ret3=rnorm(10)
#')
#'
#' a <- d %>% xkey("ticker")
#' attributes(a)$key
#'
xkey <- function(d, keys) {
  attributes(d)$key <- keys
  d
}
