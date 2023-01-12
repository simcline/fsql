#' Rename some columns of a data.frame
#'
#' @param d data.frame
#' @param ... 'what' arguments: a collection of column names definition, e.g under the form new_name = old_name. See examples below
#'
#' @return renamed data.frame
#' @export
#'
#' @examples
#'
#' d <- data.frame(
#'date=seq(as.Date("2001-01-01"), as.Date("2001-10-01"), by="month"),
#'ticker=sample(c("1 HK", "2 HK"), 10, TRUE),
#'ret1=rnorm(10),
#'ret2=rnorm(10),
#'ret3=rnorm(10)
#')
#'
#'d %>% rename(newret1 = ret1, newret2 = ret2)
#'
#' It is also possible to pass an alist as follows
#'
#' d %>% rename(newret1 = ret1, alist(newret2 = ret2, newret3 = ret3))
#'
#'or a character list
#'
#'d %>% rename(newret1 = ret1, list(newret2 = "ret2", newret3 = "ret3"))
#'
rename <- function(d, ...){
  what <- as.list(substitute(...()))

  for (i in 1:length(what)){

    if (is.null(names(what)) || names(what)[i] == ""  ){ #may be an alist or a c(var1,var2) argument
      what[[i]] <- tryCatch({y <- eval(what[[i]])
      if((is.list(y)|is.vector(y)) & (!paste0(deparse(what[[i]]), collapse="") %in% colnames(d))) y else what[[i]]
      }, error = function(e) what[[i]])
    }


  }

  to_remove <- list()
  for (i in 1:length(what)){
    if ( (is.list(what[[i]]) | is.vector(what[[i]])) & (is.null(names(what)) || names(what)[i] == ""  ) ){
      if (is.character(what[[i]]  %>% unlist)) { #names of columns in the form #c("var1","var2",...)
        what[[i]] %<>% each(parse_expr)
      }
      what <- append(what, what[[i]])
      to_remove %<>% append(i)
    }
  }

  what[to_remove %>% unlist] <- NULL

  what  %<>% each(function(x) paste0(deparse(x), collapse="") )

  for (i in 1:length(what) ){
    d[,names(what)[[i]] ] <- d[,what[[i]]]
    d[,what[[i]]] <- NULL
  }
  d

}
