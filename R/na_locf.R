#' Fill in NAs with the previous non-missing value
#'
#' Inspired by zoo::na.locf0
#'
#' @param object an object
#' @param fromLast logical. Causes observations to be carried backward rather
#'   than forward. Default is FALSE.
#'
#' @export
#' @return Original object with NAs filled in
na_locf <- function(object, fromLast = FALSE) {
  if (fromLast) object <- rev(object)
  for (i in seq_along(object)) {
    if (i > 1 && is.na(object[i])) {
      object[i] <- object[i-1]
    }
  }
  if (fromLast) object <- rev(object)
  return(object)
}
