#' Rename columns of a data.frame
#'
#' @param d data.frame
#' @param cols vector of new column names
#'
#' @return renamed data.frame
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
#'d %>% xcol(c("date" , "return1"))
xcol <- function(d, cols) {
  names <- colnames(d)
  names[1:length(cols)] <- cols
  colnames(d) <- names
  d
}
