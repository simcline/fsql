
#' returns a boolean vector indicating which values to trim
#'
#' @param x vector o trim
#' @param down threshold down
#' @param up threshold up
#'
#' @return
#' @export
#'
#' @examples
#'
#'
#' d <- data.frame(
#' date=seq(as.Date("2001-01-01"), as.Date("2001-10-01"), by="month"),
#' ticker=sample(c("1 HK", "2 HK"), 10, TRUE),
#' ret1=rnorm(10),
#' ret2=rnorm(10),
#' ret3=rnorm(10)
#' )
#'
#' d %>% filter(ret1 %>% trim(0.1,0.9) )

trim <- function(x, down=0, up=1){
  x<= quantile(x, up, na.rm=T) & x>=quantile(x,down,na.rm=T)
}
