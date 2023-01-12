#' Shortcut for select(where=..., by=...)
#'
#' @param d data.frame
#' @param ... a collection of boolean conditions on the columns
#'
#' @return the filtered data.frame
#' @export
#'
#' @examples
#'  d <- data.frame(
#' date=seq(as.Date("2001-01-01"), as.Date("2001-10-01"), by="month"),
#' ticker=sample(c("1 HK", "2 HK"), 10, TRUE),
#' ret1=rnorm(10),
#' ret2=rnorm(10),
#' ret3=rnorm(10)
#' )
#'
#' d %>% filter(ret1 >0, ret2 <0)
#'
filter <- function(d, ...){
  where <- as.list(substitute(...())) %>% to_expr_c
  env <- parent.frame()
  eval(substitute(fql_wrapper(d,where=where2, by= NULL, fql_fun = select_aux, exclude=NULL, env = env), list(where2=substitute(where), env=env )), enclos = env)
}
