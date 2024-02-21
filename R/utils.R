#' Pick first element
#'
#' @param x any structure compatible with head
#'
#' @return first element
#' @export
#'
#' @examples
#' x <- c(1,2,3)
#' first(x)
first <- function(x) {
  head(x,1)
}

#' Pick last element
#'
#' @param x any structure compatible with head
#'
#' @return last element
#' @export
#'
#' @examples
#' x <- c(1,2,3)
#' last(x)
last <- function(x) {
  tail(x,1)
}

#' Pick nth element
#'
#' @param x any structure compatible with simple-bracket slicing
#' @param n index
#'
#' @return element in position n of x
#' @export
#'
#' @examples
#' x <- c(1,2,3)
#' nth(x,2)
#' nth(x,-1)
nth <- function(x,n) {
  if (n >= 0){
    x[n]
  } else {
    x[length(x) + n + 1]
  }
}


#' today - n days in format YYYY-MM-DD
#'
#' @param n days shift from the current day. Defaults to 0.
#'
#' @return today - n days in format YYYY-MM-DD
#' @export
#'
#' @examples
#' today()
#' today(-1) # yesterday
#' today(1) # tomorrow
#' today(-7) # a week ago
today <- function(n = 0){
  as.character(Sys.Date() + n)
}

#' negative index compatible slicing of a vector or a list
#'
#' @param x anything compatible with []
#' @param start starting index (included). must be >= -length(x) and not 0
#' @param end end index (included). must be >= -length(x) and not 0
#'
#' @return the sliced vector. negative index n < 0 designates position length(x)+n+1. For instance, -1 designates length(x). index 0 is meaningless and will throw an error. If start and end have the same sign, the usual [] rule is applied. Otherwise, the vector is sliced by starting from start and 'walking up' to end assuming that the first element of x is the successor of the last element of x. See examples below.
#' @export
#'
#' @examples
#' x <- 1:10
#' x %>% subvec(-1,2) # c(10,1,2)
#' x %>% subvec(-7,-5) # c(4,5,6)
#' x %>% subvec(-5,-7) # c(6,5,4)
#' x %>% subvec(1,-7) # c(1,2,3,4)
#' x %>% subvec(1,-1) # 1:10
subvec <- function(x, start, end){
  if (length(x) == 0){
    x
  }
  if (start == 0){
    stop("start cannot be 0")
  }
  if (end == 0){
    stop("end cannot be 0")
  }
  if (start < -length(x)) {
    stop(paste0("start :", start, " is smaller than -length of argument :", -length(x) ))
  }
  if (end < -length(x)) {
    stop(paste0("end :", end, " is smaller than -length of argument :", -length(x) ))
  }

  isStartSmallerThanEnd <- start <= end

  if (start < 0){
    posStart <- length(x) + start + 1
  }

  if (end < 0){
    posEnd <- length(x) + end + 1
  }

 if (start <0 & end <0){
   x[posStart:posEnd]
 } else if (start<0 & end >0) {
   c(x[posStart:length(x)], x[1:end])
 } else if (start >0 & end <0) {
   x[start:posEnd]
 } else {
   x[start:end]
 }

}

