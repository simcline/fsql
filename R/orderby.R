#' Similar to SQL order by function
#'
#' @param d data.frame
#' @param ... columns to order. Columns in descending order should be prefixed by "-".
#'
#' @return the ordered data.frame
#' @export
#'
#' @examples
#' d <- data.frame(
#' date=seq(as.Date("2001-01-01"), as.Date("2001-10-01"), by="month"),
#' ticker=sample(c("1 HK", "2 HK"), 10, TRUE),
#' ret1=rnorm(10),
#' ret2=rnorm(10),
#' ret3=rnorm(10)
#' )
#'
#' d %>% orderby(ret1)
#' d %>% orderby(ticker, -ret1)

orderby <- function(d,...){
  what_str <- as.list(substitute(...())) %>% each(unparse) %>% unlist

  order_var <- what_str %>% each(\(x) ifelse( startsWith(x,"-"), paste0("-rank(",substr(x,2,nchar(x)),")"), paste0("rank(",x,")")))

    s <- paste0("with(d, order(", paste0(order_var, collapse=","),
               "))")
    d[eval_char(s), ]
}
