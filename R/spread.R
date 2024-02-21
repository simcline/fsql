#' Spread a column into several ones
#'
#' @param d data.frame
#' @param key the column containing labels of resulting columns
#' @param value the value column to spread
#'
#' @return the spread data.frame
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
#'u <- d %>% gather(ret1,ret2)
#'u %>% spread(key,value)
#'
spread <- function(d, key, value){

  key_str <- unparse(substitute(key))
  val_str <- unparse(substitute(value))

  d_list <- d %>% xgroup(key_str) %>% each(
    function(x){
      x[,as.character(x[1,key_str])] <- x[,val_str]
      x[,key_str]<- NULL
      x[,val_str]<- NULL
      x
    })
  rep <- d_list[[1]]
  if (length(d_list) >= 2){
    for (k in 2:length(d_list)){
      rep %<>% uj(d_list[[k]])
    }
  }
  rep
}


