xcols <- function(d, cols) {
  # cols is a subset of colnames(d)
  y <- setdiff(colnames(d), cols)
  d[, c(cols, y)]
}