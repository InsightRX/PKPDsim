#' Create lower-diagonal omega matrix from CV for parameter estimates
#'
#' @param par_cv list of parameter CVs
#' @param par list of parameters
#' @return a vector describing the lower triangle of the omega (between-subject variability) matrix
#' @export
#' @seealso \link{sim_ode}

cv_to_omega <- function(par_cv = list(), par = list()) {
  nam_cv <- names(par)
  nam <- names(par)
  omega <- c()
  for (i in 1:length(nam)) {
    if (nam[i] %in% names(par_cv)) {
      om_diag <- sqrt(par_cv[[nam[i]]])
    } else {
      om_diag <- 0
    }
    omega <- c(omega, c(rep(0, i-1), om_diag))
  }
  return(omega)
}
