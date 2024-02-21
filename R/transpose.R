#' Transpose a data.frame along some pivot variables
#'
#' @param d data.frame
#' @param ... pivot variables whose values become columns. There the n-uplets thus formed should be unique and do not repeat in the data.
#'
#' @return the transposed data.frame
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
#' d[1,] %>% transpose
#' d %>% transpose(date)
#' d %>% transpose(date, ticker)
#'
#' #these don't work:
#' d %>% transpose #pure transpose only works for one-row data.frame
#' d %>% transpose(ticker) #duplicated values in ticker
#'
transpose <- function(d, ...){
  by_str <- as.list(substitute(...())) %>% each(unparse) %>% unlist

  u <- (if (is.null(by_str)){
    if (nrow(d) == 1){
      l <- names(d) %>% each(parse_expr)
      do.call(gather, c(list(d), l))
    } else {
      stop("transposing with no pivot variable only works for one-row data.frame")
    }
  } else {
    l <- names(d) %>% setdiff(by_str) %>% each(parse_expr)
    g <- do.call(gather, c(list(d), l)) %>%
     xgroup(by_str)
    if (length(g) == nrow(d)){
      g %>%
       each(\(q) {
         q[, by_str %.>% (\(x) as.character(q[1,x])) %>% if(length(.) > 1) paste0("(", paste0(.,collapse=","), ")") else .] <- q[,"value"]
         q[,c(by_str,"value")]<-NULL
         q
       }) %>%
         Reduce(\(x,y) x %>% cbind(y[,2, drop=F]), .)
    } else {
      stop("duplicated values in pivot variables")
    }
  }) %>%
    rename(Var=key) %>%
    pull(Var)
  rownames(u) <- 1:nrow(u)
  u
}
