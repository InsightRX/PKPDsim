#' Adapted from zoo::na.locf0
#'
#' @param object an object
#' @param fromLast logical. Causes observations to be carried backward rather than forward. Default is FALSE. With a value of TRUE this corresponds to NOCB (next observation carried backward). It is not supported if x or xout is specified.
#'
#' @export
na_locf <- function(object, fromLast = FALSE) {
  if (fromLast) object <- rev(object)
  ok <- which(!is.na(object))
  if(is.na(object[1L])) ok <- c(1L, ok)
  gaps <- diff(c(ok, length(object) + 1L))
  object <- rep(object[ok], gaps)
  if (fromLast) object <- rev(object)
  return(object)
}
