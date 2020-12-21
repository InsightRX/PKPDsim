#' ifelse function but then based on whether value is NULL or not
#'
#' @param value metadata list object
#' @param alternative alternative value
#' @param allow_null can the alternative be NULL?
#'
#' @export
ifelse0 <- function(value = NULL, alternative = NULL, allow_null = FALSE) {
  if(is.null(alternative) && !allow_null) {
    stop("No alternative specified")
  }
  if(!is.null(value)) {
    return(value)
  } else {
    return(alternative)
  }
}
