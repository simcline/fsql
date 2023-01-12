#' a shortcut for parse(text=...)[[1]]
#'
#
#'
#' @param t text to parse
#'
#' @return the parsed expression
#' @export
#' @examples
#' x<-parse_expr("a=2")
#' eval(x)
parse_expr <- function(t) {
  parse(text=t)[[1]]
}

#' Eval a character seens as an expression in the given environment
#'
#' @param s a char string
#' @param env an environment. Default is parent.frame()
#'
#' @return
#' @export
#'
#' @examples
#' x<-2
#' eval_char("x^2 + 6")
eval_char <- function(s, env = parent.frame()){
  eval(parse_expr(s), env)
}

#'  Deparse x, but avoid introducing artificial breaks in the returned character string
#'
#' @param x value to deparse
#' @export
#' @return deparse(x) with no breaks
#'

unparse <- function(x){
  paste0(deparse(x),collapse="")
}


parse_c <- function(x){

  x <- unparse(x)

  if ((x %>% startsWith("c(")) & (x %>% endsWith(")"))){

    x_tab <-  x %>% strsplit("") %>% .[[1]]
    x_count <- x_tab %>% {(cumsum(. == "(") - cumsum(. == ")"))}
    x_tab[x_count !=1 ] <- gsub(",", "%%%", x_tab[x_count!=1])

    x_list <- x_tab %>% paste0(collapse="")
    x_list %<>% substr(3,nchar(x_list) -1) %>%
      strsplit(",") %>% .[[1]] %>% each(function(t) gsub("%%%", ",", t))
    x_list %<>% each(parse_expr)

  } else {
    x_list <- list(x %>% parse_expr)
  }
  names(x_list) <- 1:length(x_list) %>% each(function(x) ifelse(is.symbol(x_list[[x]]), unparse(x_list[[x]]), ""))
  x_list

}


to_expr_c <- function(x){
  x <- paste0("c(", paste(x %>% each(function(y) unparse(y)), sep="", collapse = ',') , ")")
  parse_expr(x)
}


