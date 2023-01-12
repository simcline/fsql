onCall <- function(f, handler=function(args) cat(as.character(args), "\n")) {
  function(...) {
    args <- list(...)
    handler(args)
    do.call(f, args)
  }
}
