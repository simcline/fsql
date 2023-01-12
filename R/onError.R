onError <- function(f, handler=function(error) {cat(as.character(error));NULL}) {
  function(...) {
    args <- list(...)
    tryCatch(do.call(f, args), error=handler)
  }
}
