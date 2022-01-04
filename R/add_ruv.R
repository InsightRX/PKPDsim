#' Add residual variability to the dependent variable
#'
#' @param x dependent value without residual variability
#' @param ruv list specifying proportional, additive and/or exponential errors (`prop`, `add`, `exp`)
#' @param obs_type vector of observation types
#'
#' @export
#' @return Input vector with residual variability added
add_ruv <- function(x, ruv = list(), obs_type = 1) {
  if(!is.null(ruv$prop)) {
    x <- x * (1 + stats::rnorm(length(x), 0, ruv$prop[obs_type]))
  }
  if(!is.null(ruv$add)) {
    x <- x + stats::rnorm(length(x), 0, ruv$add[obs_type])
  }
  if(!is.null(ruv$exp)) {
    x <- x * exp(stats::rnorm(length(x), 0, ruv$exp[obs_type]))
  }
  return(x)
}
