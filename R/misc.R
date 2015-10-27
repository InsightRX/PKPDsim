#' covariate function builder
#'
#' @param ... parameters to pass to cov
#'
#' @export
f_cov <- function (...) {
  substitute( with(cov, { ... } ) )
}

cumhaz_to_surv <- function(cumhaz) {
  exp(-cumhaz)
}

get_size_ode <- function(ode, p) {
  p$dose_times <- c(0)
  p$dose_amts  <- c(0)
  p$rate <- 1
  dum <- ode(1, rep(1, 1000), p)
  length(dum[[1]])
}
