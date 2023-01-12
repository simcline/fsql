#' SQL-like update function with internal "where" and "by" features
#'
#' @param d data.frame
#' @param ... 'what' arguments: a collection of column definitions. See examples below
#' @param where boolean condition on columns, under the form cond1 if only one condition, and c(cond1, cond2, ...) if more than one condition
#' @param by collection of columns, under the form col1 if only one condition, and c(col1, col2, ...) if more than one column
#'
#'
#' @return updated data.frame
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
#'d %>% update(vol = mean(ret1^2))
#'
#'d %>% update(vol1 = mean(ret1^2), vol2 = mean(ret2^2), where = c(date!="2001-05-01", ret3 >0), by =ticker)
#'
update <- function(d, ..., where = NULL, by = NULL){

  env <- parent.frame()

  eval(substitute(fql_wrapper(d, ..., where = where2, by = by2, fql_fun = update_aux, env= env), list(where2= substitute(where), by2=substitute(by), env= env)),
       enclos = env)

}


#' Recycling value in given environment, for internal use
#'
#' @param x
#' @param cols
#' @param env
#'
#' @return
#'
#' @examples
recycle <- function(x, cols, env) {
  l     <- lapply(cols, function(y) eval(y, env = x, enclos = env))
  data.frame(l)
}

#' Auxiliary update function, for internal use
#'
#' @param d
#' @param where
#' @param by
#' @param what
#' @param env
#'
#' @return
#'
#' @examples
update_aux <-  function (d, where = NULL, by = NULL, what = NULL, env = NULL)
{
  if (is.null(env)) {env <- parent.frame()}
  if (is.null(d))
    return(d)
  if (nrow(d) == 0)
    return(d)

  if ((length(where) == 0) | is.null(where)) {
    where <- list(rep(TRUE, nrow(d)))
  }
  if (length(what) == 0 | is.null(what)) {
    if (!is.null(by)) {
      what <- lapply(colnames(d), function(x) parse(text = paste("tail(",
                                                                 x, ",1)", sep = "")))
      names(what) <- colnames(d)
    }
    else {
      what <- lapply(colnames(d), function(x) parse(text = x))
      names(what) <- colnames(d)
    }
  }
  if (!is.null(by)) {
    bye <- by
    nonby <- setdiff(colnames(d), names(bye))
    bye[nonby] <- lapply(nonby, function(x) parse(text = x))
  }
  else {
    bye <- lapply(colnames(d), function(x) parse(text = x))
    names(bye) <- colnames(d)
  }
  if(!is.null(by)) {
    by[names(by)]   <- lapply(names(by), function(x) parse(text=paste("tail(", x, ", 1)", sep="")))
    what            <- c(what, by)
  }
  cols <- lapply(colnames(d), function(x) parse(text = x))
  names(cols) <- colnames(d)
  cols[names(what)] <- what
  i <- Reduce("&", lapply(where, function(x) eval(x, env = d,
                                                  enclos = env)))
  j <- dict(1:nrow(d), i)
  e <- d
  for (j in names(cols)) {
    if (!j %in% colnames(d)) {
      e[, j] <- NA
    }
  }
  if (nrow(d[i, , FALSE]) == 0) {
    return(d)
  }

  x <- d[i, , FALSE] %>% (function(x) data.frame(lapply(bye,
                                                        function(y) eval(y, env = x, enclos = env)), stringsAsFactors = FALSE)) %>%
    xgroup(names(by)) %>% each(function(x) recycle(x, cols, env)) %>%
    ungroup
  e[i, ] <- x
  for(i in 1:ncol(e)) {
    class(e[, i]) <- class(x[, i])
  }
  if(!is.null(by)) {
    return(e %>% xcols(names(by)) %>% xasc(names(by)))
  } else {
    return(e)
  }
}
