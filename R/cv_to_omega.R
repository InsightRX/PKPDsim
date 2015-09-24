#' Create lower-diagonal omega matrix from CV for parameter estimates
#'
#' @param par_cv list of parameter CVs
#' @param parameters list of parameters
#' @return a vector describing the lower triangle of the omega (between-subject variability) matrix
#' @export
#' @seealso \link{sim_ode}

cv_to_omega <- function(par_cv = NULL, parameters = NULL) {
  if (is.null(par_cv)) {
    stop("List of coefficients of variation required as argument to cv_to_omega().")
  }
  if (is.null(parameters)) {
    stop("Parameter list required as argument to cv_to_omega().")
  }
  nam <- names(parameters)
  omega <- c()
  for (i in 1:length(nam)) {
    if (nam[i] %in% names(par_cv)) {
      om_diag <- (par_cv[[nam[i]]])^2
    } else {
      om_diag <- 0
    }
    omega <- c(omega, c(rep(0, i-1), om_diag))
  }
  return(omega)
}
