
#' SQL like select function with internal "where" and "by" features
#'
#' @param d data.frame
#' @param ... 'what' arguments: a collection of column names (or colums definitions). See examples below
#' @param where boolean condition on columns, under the form cond1 if only one condition, and c(cond1, cond2, ...) if more than one condition
#' @param by collection of columns, under the form col1 if only one condition, and c(col1, col2, ...) if more than one column
#'
#' @return selected data.frame
#' @export
#'
#' @examples
#'
#'d <- data.frame(
#'date=seq(as.Date("2001-01-01"), as.Date("2001-10-01"), by="month"),
#'ticker=sample(c("1 HK", "2 HK"), 10, TRUE),
#'ret1=rnorm(10),
#'ret2=rnorm(10),
#'ret3=rnorm(10)
#')
#'
#' #The syntax is similar to that of **dplyr**.
#'
#'d %>% select(date, ret1)
#'
#'d %>% select(-date)
#'
#' #We can also select by giving a vector/list of strings:
#'
#'d %>% select(c("ret1", "ret2", "date"))
#'
#' #**Be careful**: the name of a column in d **always masks** any variable in the global environment, hence:
#'
#'mycols <- c("ret1", "ticker")
#'d %>% select(mycols)
#'
#' #works, but
#' ret2 <- c("ret1", "ticker")
#' d %>% select(ret2)
#'
#' #does not yield the expected result (or rather, you should expect to obtain the above result!). **where** and **by** options work as follows.
#'
#' d %>% select(ret1 = mean(ret1), where = c(ret1 >0, ret2>0), by = ticker)
#'
select <- function(d, ..., where = NULL, by = NULL){

  what_str <- as.list(substitute(...())) %.>% unparse

  if (length(what_str) !=0){
    if (is.null(names(what_str))){
      keep <- what_str[what_str %.>% (\(x) !startsWith(x,"-"))]
      exclude <- what_str[what_str %.>% (\(x) startsWith(x,"-"))] %.>% (\(x) substr(x, 2, nchar(x)))
    } else {
      keep <- what_str[ (what_str %.>% (\(x) !startsWith(x,"-"))) | (names(what_str) != "") ]
      exclude <- what_str[(what_str %.>% (\(x) startsWith(x,"-"))) & (names(what_str) == "")] %.>% (\(x) substr(x, 2, nchar(x)))
    }
  } else {
    keep <- NULL
    exclude <- NULL
  }

  env <- parent.frame()
  if (!is.null(keep)) {
    keep %<>% each(parse_expr)

    eval(substitute(do.call(fql_wrapper,
                            c(list(d), keep, alist(where = where2, by = by2, fql_fun = select_aux, exclude=exclude),list(env=env) )),
                    list(keep = substitute(keep), where2= substitute(where), by2=substitute(by), env=env)),
         enclos =env)
  } else {
    eval(substitute(do.call(fql_wrapper,
                            c(list(d), alist(where = where2, by = by2, fql_fun = select_aux, exclude=exclude), list(env=env) )),
                    list(where2= substitute(where), by2=substitute(by), env=env)),
         enclos = env)
  }
}


#' Auxiliary select function for internal use
#'
#' @param d
#' @param where
#' @param by
#' @param what
#' @param env
#'
#' @return
#'
#'
#'
select_aux <- function(d, where = NULL, by = NULL, what = NULL, env= NULL) {
  if (is.null(env)) env <- parent.frame()

  if ((length(where) == 0) | is.null(where)) {
    where <- list(rep(TRUE, nrow(d)))
  }
  if (length(what) == 0 | is.null(what)) {
    if (!is.null(by)) {
      what        <- lapply(colnames(d), function(x) parse(text = paste("tail(", x, ", 1)", sep = "")))
      names(what) <- colnames(d)
    }
    else {
      what        <- lapply(colnames(d), function(x) parse(text = x))
      names(what) <- colnames(d)
    }
  }
  if(length(names(what))==0) {
    if(all(what %>% each(class) %>% unlist =="name")) {
      names(what) <- unlist(what)
    }
  }
  if(length(names(by))==0) {
    if(all(by %>% each(class) %>% unlist =="name")) {
      names(by) <- unlist(by)
    }
  }
  if (!is.null(by)) {
    bye   <- by
    nonby <- setdiff(colnames(d), names(bye))
    if (length(nonby) > 0) {
      bye[nonby] <- lapply(nonby, function(x) parse(text = x))
    }
  }
  else {
    bye        <- lapply(colnames(d), function(x) parse(text = x))
    names(bye) <- colnames(d)
  }
  r <- d[Reduce("&", lapply(where, function(x) eval(x, env = d, enclos = env))), , FALSE]
  if (nrow(r) == 0) {
    return(r)
  }
  if(!is.null(by)) {
    by[names(by)]   <- lapply(names(by), function(x) parse(text=paste("tail(", x, ", 1)", sep="")))
    what            <- c(what, by)
    what            <- what[!(as.character(what) %>% duplicated())]
  }
  r <- r %>%
    (function(x) data.frame(lapply(bye, function(y) eval(y, env = x, enclos = env)), stringsAsFactors = FALSE)) %>%
    xgroup(names(by)) %>%
    each(function(x) data.frame(lapply(what, function(y) eval(y, env = x, enclos = env)), stringsAsFactors = FALSE)) %>%
    ungroup
  if(!is.null(by)) {
    return(r %>% xcols(names(by)) %>% xasc(names(by)))
  } else {
    return(r)
  }
}


