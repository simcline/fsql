#' wrapper function to pipe non standard arguments to fql style select/update. Should not be used directly
#'
#' @param d data.frame
#' @param ... 'what' arguments: a collection of column names or column definitions
#' @param where boolean condition on columns, under the form cond1 if only one condition, and c(cond1, cond2, ...) if more than one condition
#' @param by collection of columns, under the form col1 if only one condition, and c(col1, col2, ...) if more than one column
#' @param fql_fun the fql_fun to apply to d
#' @param exclude a character vector containing columns to exclude
#'
#' @return the output data.frame

fql_wrapper <- function(d, ..., where, by, fql_fun, exclude = NULL, env = NULL){

  if (is.null(env)) env <- parent.frame()

  if (unparse(substitute(...))[1]!="NULL"){
    what <- as.list(substitute(...()))

    for (i in 1:length(what)){

      if (is.null(names(what)) || names(what)[i] == ""  ){ #may be an alist or a c(var1,var2) argument
        what[[i]] <- tryCatch({y <- eval(what[[i]])
        if((is.list(y)|is.vector(y)) & (!unparse(what[[i]]) %in% colnames(d))) y else what[[i]]
        }, error = function(e) what[[i]])
      }


    }

    to_remove <- list()
    for (i in 1:length(what)){
      if ( (is.list(what[[i]]) | is.vector(what[[i]])) & (is.null(names(what)) || names(what)[i] == ""  ) ){
        if (is.character(what[[i]])) { #names of columns in the form #c("var1","var2",...)
          what[[i]] %<>% each(parse_expr)
        }
        what <- append(what, what[[i]])
        to_remove %<>% append(i)
      }
    }

    what[to_remove %>% unlist] <- NULL

  } else {
    what <- NULL
  }

  if (!is.null(exclude)){
    what_str <- what %>% each(unparse) %>% unlist

    if (identical(colnames(d), setdiff(colnames(d), what_str))){
      what_str %<>% union(setdiff(colnames(d), exclude))
    } else {
      what_str %>% setdiff(exclude)
    }

    for (i in 1:length(what)){
      if (!unparse(what[[i]]) %in% what_str) what[[names(what)[[i]]]]<-NULL
    }

    what_str %<>% setdiff(what %>% each(unparse) %>% unlist)

    what %<>% append(what_str %>% each(parse_expr))
  }

  if (!is.null(what)){
    names(what) <- 1:length(what) %>% each(function(x) ifelse( is.null(names(what[x]))||length(names(what[x]))==0||names(what[x])=="", unparse(what[[x]]), names(what[x]) ))
  }

  if (unparse(substitute(by))!="NULL"){
    by <- parse_c(substitute(by))
  }
  if (unparse(substitute(where))!="NULL"){
    where <- parse_c(substitute(where))
  }

  eval(substitute(do.call(fql_fun,
                          c(list(d), alist(what= what2, where = where2, by = by2, env=env))),
                  list(what2 = substitute(what),where2= substitute(where), by2=substitute(by), env=env)),
       enclos = env)
}


