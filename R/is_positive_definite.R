#' Is matrix positive definite
#'
#' @param x matrix, specified either as `vector` of lower triangle, or full matrix (as `matrix` class)
#' @export
#' @return TRUE if `x` is positive definite; FALSE otherwise.
#' @md
is_positive_definite <- function(x) {
  if(! "matrix" %in% class(x)) x <- triangle_to_full(x)
  eig <- eigen(x, only.values = TRUE)
  if (any(is.complex(eig$values))) {
    if (any(Im(eig$values) >= 1e-15)) {
      return(FALSE)
    }
    eig$values <- as.numeric(eig$values)
  }
  if(any(eig$values <= 0)) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}
