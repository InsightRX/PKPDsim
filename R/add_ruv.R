#' Add residual variability to the dependent variable
#'
#' @param x dependent value without residual variability
#' @param ruv list specifying proportional, additive and/or exponential errors (`prop`, `add`, `exp`)
add_ruv <- function(x, ruv = list()) {
  if(!is.null(ruv$prop)) {
    x <- x * (1 + rnorm(length(x), 0, ruv$prop))
  }
  if(!is.null(ruv$prop)) {
    x <- x + rnorm(length(x), 0, ruv$add)
  }
  if(!is.null(ruv$exp)) {
    x <- x * exp(ruv$exp)
  }
  return(x)
}
