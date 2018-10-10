#' Is matrix positive definite
#'
#' @param x matrix, specified either as `vector` of lower triangle, or full matrix (as `matrix` class)
#' @export
is_positive_definite <- function(x) {
  if(! "matrix" %in% class(x)) x <- triangle_to_full(x)
  eig <- eigen(x)
  if(any(eig$values <= 0)) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}
