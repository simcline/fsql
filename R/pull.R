
#' pulls ccolumns to the left of a datra.frame
#'
#' @param d data frame
#' @param ... columns
#'
#' @return the reorganized data frame
#' @export
#'
#' @examples
#'
#'   d <- rbind(data.frame(date="20140101", ticker= 2, price=3),
#'   data.frame(date="20140102", ticker= 2, price=2),
#'   data.frame(date="20140101", ticker= 1, price=5))
#'   d %>% pull(ticker, price)
pull <- function(d,...) {
  what_str <- as.list(substitute(...())) %>% each(unparse) %>% unlist
  xcols(d, what_str)
}
