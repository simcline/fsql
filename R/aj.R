#' As of join
#' @param by a collection of column names. See examples below
#' @param x  data.frame
#' @param y  data.frame
#' @return dataframe which is joining y to x as of by columns
#' @export
#' @examples
#'
#' op <- options(digits.secs = 6)
#' d1 <- data.frame(ts = c(Sys.time(),Sys.time()+1,Sys.time()+3), bid = c(100,99,100))
#' d2 <- data.frame(ts = c(Sys.time(),Sys.time()+2,Sys.time()+2), ask = c(101,100,102))
#' aj(ts,x=d1,y=d2)
aj <- function(..., x, y) {
  by_str <- as.list(substitute(...())) %>% each(function(x) paste0(deparse(x),collapse="") ) %>% unlist

  tofill         <- setdiff(colnames(y), by_str)
  filling        <- lapply(tofill, function(x) parse(text=paste("fills(", x, ")")))
  names(filling) <- tofill
  by           <- head(as.list(substitute(...())), -1)

  l <- y %>%
    xasc(by_str) %>%
    uj(x[, by_str, drop=FALSE] %>%
         xkey(by_str)) %>%
    xasc(by_str)
  if(length(by)==0) {
    l <- eval(substitute(do.call(update,
                                c(list(l), filling)),
                                list(filling = substitute(filling))),
              enclos = parent.frame())

  } else {
    l <- eval(substitute(do.call(update,
                                 c(list(l), filling, alist(by = by2))),
                         list(filling = substitute(filling), by2 = substitute(by))),
              enclos = parent.frame())
  }
  x %>% lj(l %>% xkey(by_str))
}

