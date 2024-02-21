#' Gather several columns into one
#'
#' @param d data.frame
#' @param ... colum names to gather
#' @param key a character name for the column containing the column names given in ...
#' @param value a character name for the column containing the values of the columns given in ...
#'
#' @return the transformed data.frame
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
#'d %>% gather(ret1,ret2)
#'
#'d %>% gather(ret1,ret2,ret3, key="retType", value="retValue")
#'
gather <- function(d, ..., key = "key", value = "value"){
  what_str <- as.list(substitute(...())) %>% each(unparse) %>% unlist

  t <- if (what_str %.>% (\(x) !is.numeric(d[,x])) %>% any) as.character else (\(x) x)

  d_list <- list()
  for (x in what_str){
    d_list[[x]] <- d[, c(setdiff(names(d),what_str),x), drop=F]
    d_list[[x]][,key] <- x
    d_list[[x]][,value] <- t(d[,x])
    d_list[[x]][,x] <- NULL
  }

  d_out <- NULL
  for (y in d_list){
    d_out %<>% rbind(y)
  }
  d_out
}
