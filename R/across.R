#' Apply a function to a set of columns (should be plugged in a select or an update)
#'
#' @param f function to apply
#' @param ... column names
#' @param bylist column names, if in a list and characters
#' @param pref string prefix to apply to the newly created column names
#'
#' @return an alist containing the recipe to construct the new columns
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
#' d %>% select(across(mean, ret1, ret2, pref="av_"), by = ticker)
#'
#'mycols <- c("ret1", "ret2")
#'d %>% select(across(mean, bylist = mycols, pref="av_"), by = ticker)

across <- function(f,..., bylist = NULL, pref = ""){

  what_str <- as.list(substitute(...())) %>% each(unparse) %>% unlist
  what_str %<>% append(bylist)
  fun_name <- paste0("(",unparse(substitute(f)),")")

  formula <- what_str %>% each(function(t){
    parse_expr(paste0(fun_name, "(", t, ")"))
  })

  names(formula) <- what_str %>% each(function(t) paste0(pref,t) )
  formula

}
