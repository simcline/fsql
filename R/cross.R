#' cross is a cartesian product application of a function
#' @param x a vector
#' @param y is a vector
#' @param FUN a binary function
#' @return dataframe of application of FUN to cartesian product of x and y
cross <- function(x, y, FUN=function(x,y) c(x,y), reduce=NULL) {
  index <- expand.grid(1:length(x), 1:length(y)) %>% t %>% data.frame %>% as.list
  l <- lapply(index, function(z, x, y) FUN(x[z[1]], y[z[2]]), x=x, y=y)
  if(!is.null(reduce)) {
    return(l %>% reduce)
  } else {
    return(l)
  }
}
