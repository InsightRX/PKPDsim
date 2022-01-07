#' Print function for PKPDsim covariate object
#'
#' @param x covariate object
#' @param ... additional arguments
#' @return No return value, print function.
#' @export
print.covariate <- function(x, ...) {
  if(! "covariate" %in% class(x)) {
    stop("Not a PKPDsim covariate object")
  }
  tmp <- as.data.frame(x[c("value","times")])
  tmp$unit <- x$unit
  tmp$implementation <- x$implementation
  print(tmp)
}
