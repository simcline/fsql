#' Merge a list of data.frame having the same keys and columns
#'
#' @param l list of data.frame to merge
#'
#' @return merged data.frame
#' @export
#'
#' @examples
ungroup <- function(l) {
  do.call(rbind, l)
}
