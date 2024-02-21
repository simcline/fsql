#' split the dataframe into subgroups w.r.t some columns
#'
#' @param d data.frame
#' @param ... columns
#'
#' @return list of data.frames
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
#'
#' d %>% group(ticker)
group <- function(d,...){
  what_str <- as.list(substitute(...())) %>% each(unparse) %>% unlist
  d %>% xgroup(what_str)
}
