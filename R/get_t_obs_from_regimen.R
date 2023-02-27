#' Extract sensible default observation times from a specified regimen
#'
#' @param regimen regimen created using `new_regimen()`
#' @param obs_step_size step size between observations. Will be auto-calculated if NULL
#' @param t_max max time value
#' @param covariates covariates object, created using `list(new_covariate(), ...)`
#' @param extra_t_obs add timepoints to t_obs at which covariate is changing (`T`/`F`)
#' @param t_init time of initiation of the ODE system. Usually 0.
#' @keywords internal
get_t_obs_from_regimen <- function(
  regimen = NULL,
  obs_step_size = NULL,
  t_max = NULL,
  covariates = NULL,
  extra_t_obs = NULL,
  t_init = 0) {

  if(is.null(obs_step_size)) {
    obs_step_size <- 1
    if(length(regimen$dose_times) == 1 && regimen$dose_times == 0) {
      obs_step_size <- 1
      } else {
      obs_step_size <- 100
      if(max(regimen$dose_times) < 10000) { obs_step_size <- 100 }
      if(max(regimen$dose_times) < 1000) { obs_step_size <- 10 }
      if(max(regimen$dose_times) < 250) { obs_step_size <- 1 }
      if(max(regimen$dose_times) < 10) { obs_step_size <- .1 }
    }
  }
  if("regimen" %in% class(regimen)) {
    if(!is.null(t_max)) {
      t_obs <- seq(from=-t_init, to=t_max, by=obs_step_size)
    } else {
      if(length(regimen$dose_times) == 1 && regimen$dose_times == 0) {
        t_obs <- seq(from=regimen$dose_times[1], to=24, by=obs_step_size)
      } else {
        t_obs <- seq(from=-t_init, to=max(regimen$dose_times) + utils::tail(regimen$interval,1), by=obs_step_size)
      }
    }
    t_obs <- unique(c(t_obs, regimen$dose_times))
    if(any(regimen$type == "infusion")) {
      t_obs <- unique(c(t_obs, regimen$t_inf[regimen$type == "infusion"]))
    }
  }
  ## add timepoints at which covariate is changing to t_obs:
  if(extra_t_obs) {
    func <- function(x) { return(x$times) }
    if(!is.null(covariates) && !is.null(covariates$times)) {
      t_obs <- unique(c(t_obs, unique(unlist(lapply(covariates, func )))))
      t_obs <- t_obs[order(t_obs)]
    }
  }
  t_obs <- round(t_obs, 6)
  return(t_obs)
}
