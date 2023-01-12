#' Tensor product to construct parameter grids (similar to expand.grid)
#'
#' @param x a vector, list or data.frame
#' @param y a vector, list or data.frame
#'
#' @return a data.frame representing the tensor product of x %.% y
#' @export
#'
#' @examples
#'
#' 1:3 %.% 1:3 #similar to expand.grid(1:3, 1:3)
#'
#' 1:3 %.% 1:3 %.% 1:4
`%.%` <- function(x,y){

   if (is.null(x)) {
      return(y)
   }
   if (is.null(y)) {
      return(x)
   }
   if (is.vector(x) | (is.list(x) & !is.data.frame(x))){

     if (is.vector(y) | (is.list(y) & !is.data.frame(y))){

        expand.grid(x,y)
     } else {
        temp <- x %>% each(function(t) {
           z <- y
           z[,paste0("Var", length(names(z)) +1)] <- t
           z})
        temp
        rbind %>% Reduce(temp)

        }
   } else {
      y <- as.data.frame(y)
      temp <- y %>% each(function(t) {
         z <- x
         z[, 1:ncol(y) %>% each(function(i) paste0("Var", ncol(z) +i)) %>% unlist] <- t
         z})
      temp
      rbind %>% Reduce(temp)
     }
   }


