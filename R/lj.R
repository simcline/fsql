
#' SQL-like left join
#'
#' @param x left data.frame
#' @param y right data.frame
#' @param keep.y
#'
#' @return merged data.frame
#' @export
#'
#' @examples
lj <- function(x, y, keep.y=FALSE) {
  if(keep.y) {
    r    <- merge(x,y, by=attributes(y)$key, all=TRUE) %>% unique
  } else {
    r    <- merge(x,y, by=attributes(y)$key, all.x=TRUE) %>% unique
  }
  # collapse columns
  t        <- colnames(r) %>% (function(x) x %>% dict(gsub(pattern="\\.x|\\.y", replacement="", x)))
  l        <- lapply(unique(t), function(x) names(t)[t==x])
  names(l) <- unique(t)
  l <- lapply(l, function(x) {
    if(length(x)==1) {
      return(r[, x])
    } else {
      colx <- grep("\\.x", x, value=TRUE)
      coly <- grep("\\.y", x, value=TRUE)
      # make sure factors do not get coerced into integers
      if(is.factor(r[,colx])) {
        isFactor=TRUE
        r[,coly] <- as.character(r[, coly])
        r[,colx] <- as.character(r[, colx])
      } else {
        isFactor <- FALSE
      }
      r <- ifelse(is.na(r[,coly]), r[,colx], r[, coly])
      if(isFactor) {
        r <- as.factor(r)
      }
      return(r)
    }
  })
  as.data.frame(l, stringsAsFactors=FALSE)
}
