#' SQL-like union join
#'
#' @param x data.frame
#' @param y data.frame
#'
#' @return the joined data.frame
#' @export
#'
#' @examples
uj <- function(x, y) {
  if(is.null(attributes(y)$key) & is.null(attributes(x)$key)) {
    return(merge(x,y, all=TRUE))
  } else {
    return(lj(x, y, keep.y=TRUE))
  }
}
