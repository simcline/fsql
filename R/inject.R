
#' Injection operator for dynamical creation of columns as parametrized images of a function (should be plugged in a select or an update)
#'
#' @param x a list/vector or data.frame corresponding to the grid of parameters
#' @param y the parametrized function partially evaluated on columns under the form f(col1, col2,...)
#'
#' @return the alist containing the recipe to construct the new columns
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
#' f <-function(x,y,k) (x*y)^k
#' d %>% select(1:5 %>>% f(ret1,ret2))
#'
#' f <-function(x,y,k,l) (x*y)^(k+l)
#' d %>% select(1:2 %.% 1:3 %>>% f(ret1,ret2))


`%>>%` <- function(x,y){
  str_expr <- unparse(substitute(y)) %>%
    strsplit(" ") %>%
    .[[1]] %>%
    paste0(collapse="")


  fun_name <- strsplit(str_expr, "(", fixed=T)[[1]][1]
  body <- strsplit(str_expr, "(", fixed=T)[[1]][2]
  body %<>% substr(start=1, stop = nchar(body)-1) %>%
    strsplit(",") %>%
    .[[1]] %>%
    paste(collapse = "_", sep="")
  z <- x %>% each(function(t) paste0(substr(str_expr,start=1, stop=nchar(str_expr)-1), ",", paste0(as.character(t), collapse=','),")"))

  if (length(names(x)) == 0 | is.data.frame(x)){
    names(z) <- x %>% each(function(t) paste(body, fun_name, paste0(as.character(t), collapse="_"), sep="_")) %>% unlist
  } else {
    names(z) <- names(x) %>% each(function(nam) paste(body, nam, sep="_")) %>% unlist
  }
  z %>% each(parse_expr)

}

