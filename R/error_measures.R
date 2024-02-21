#' medae
#'
#' @param x
#' @param y
#'
#' @return
#' @export
#'
#' @examples
medae <- function(x,y) {
  median(abs(x-y))
}

#' relmedae
#'
#' @param x
#' @param y
#'
#' @return
#' @export
#'
#' @examples
relmedae <- function(x,y) {
  median(abs(1-y/x))
}

#' normmedae
#'
#' @param x
#' @param y
#'
#' @return
#' @export
#'
#' @examples
normmedae <- function(x,y) {
  median(abs(y-x))/median(x)
}

#' lp
#'
#' @param x
#' @param y
#' @param p
#'
#' @return
#' @export
#'
#' @examples
lp <- function(x,y,w=NULL,p=1) {
  (wmean(abs(y-x)^p,w))^(1/p)
}

#' rellp
#'
#' @param x
#' @param y
#' @param p
#'
#' @return
#' @export
#'
#' @examples
rellp <- function(x,y,w=NULL,p=1) {
  (wmean(abs(1-y/x)^p,w))^(1/p)
}

#' normlp
#'
#' @param x
#' @param y
#' @param p
#'
#' @return
#' @export
#'
#' @examples
normlp <- function(x,y,w=NULL,p=1) {
  (wmean(abs(y-x)^p,w)/wmean(abs(x,w)^p))^(1/p)
}



#' rmse
#'
#' @param x
#' @param y
#'
#' @return
#' @export
#'
#' @examples
rmse <- function(x,y,w=NULL) {
  lp(x,y,w,2)
}

#' relrmse
#'
#' @param x
#' @param y
#'
#' @return
#' @export
#'
#' @examples
relrmse <- function(x,y,w=NULL) {
  rellp(x,y,w,2)
}


#' normrmse
#'
#' @param x
#' @param y
#'
#' @return
#' @export
#'
#' @examples
normrmse <- function(x,y,w=NULL) {
  normlp(x,y,w,2)
}


#' mae
#'
#' @param x
#' @param y
#'
#' @return
#' @export
#'
#' @examples
mae <- function(x,y,w=NULL) {
  lp(x,y,w,1)
}

#' relmae
#'
#' @param x
#' @param y
#'
#' @return
#' @export
#'
#' @examples
relmae <- function(x,y,w=NULL) {
  rellp(x,y,w,1)
}


#' normmae
#'
#' @param x
#' @param y
#'
#' @return
#' @export
#'
#' @examples
normmae <- function(x,y,w=NULL) {
  normlp(x,y,w,1)
}
