#' similar to the 'on' statement in SQL
#'
#' @param d data.frame
#' @param ... column names
#'
#' @return a keyed data.frame (see xkey function)
#' @export
#'
#' @examples
#' d <- data.frame(
#'  date=seq(as.Date("2001-01-01"), as.Date("2001-10-01"), by="month"),
#'  ticker=sample(c("1 HK", "2 HK"), 10, TRUE),
#'  ret1=rnorm(10),
#'  ret2=rnorm(10),
#'  ret3=rnorm(10)
#')
#'
#' x <- data.frame(ticker = c("1 HK", "2 HK"), ndeals = c(100,500))
#' d %>% lj(x %>% on(ticker))
on <- function(d,...){
  what_str <- as.list(substitute(...())) %>% each(unparse) %>% unlist
  d %>% xkey(what_str)
}
