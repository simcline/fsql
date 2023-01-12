fill_short_gaps <- function(x, fill, maxgap) {
  if (maxgap <= 0)
    return(x)
  if (maxgap >= length(x))
    return(fill)
  naruns <- rle(is.na(x))
  naruns$values[naruns$lengths > maxgap] <- FALSE
  naok <- inverse.rle(naruns)
  ifelse(naok, fill, x)
}

# Uniform function that is used to forward fill a list containing nulls
fills <- function (object, na.rm = FALSE, maxgap = Inf) {
  na.locf.0 <- function(x) {
    L <- !is.na(x)
    idx <- c(NA, which(L))[cumsum(L) + 1]
    na.index <- function(x, i) {
      L <- !is.na(i)
      x[!L] <- NA
      x[L] <- x[i[L]]
      x
    }
    xf <- na.index(x, idx)
    fill_short_gaps(x, xf, maxgap = maxgap)
  }
  if(is.data.frame(object)) {
    return(as.data.frame(lapply(object,fills), stringsAsFactors=FALSE))
  } else {
    object[] <- if (length(dim(object)) == 0) na.locf.0(object)
    else apply(object, length(dim(object)), na.locf.0)
    return(object)
  }
  
}
