#' Convert triangle omega matrix to full omega matrix
#'
#' @param vect vector specifying triangle omega matrix
#' @export
#' @return Omega matrix
triangle_to_full <- function (vect) {
  nrows <- lower_triangle_mat_size (vect)
  k_given_i_j <- function(x , y ) ifelse( y<x, x*(x-1)/2 + y, y*(y-1)/2 + x )
  k_mat <- function(p) outer( 1:p, 1:p, k_given_i_j )
  return (matrix(vect[ k_mat( nrows ) ] , nrow = nrows ))
}

#' Size of the lower triangle of the matrix
#'
#' @param mat omega matrix as a vector
lower_triangle_mat_size <- function (mat) {
  x <- length(mat)
  i <- 1
  while (x > 0) {
    x <- x-i
    i <- i+1
  }
  return(i-1)
}
