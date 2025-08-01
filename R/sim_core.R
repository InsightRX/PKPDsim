#' Only core function of the simulation function, always just returns observations.
#' Mostly useful for estimations / optimal design. Has no checks (for speed)!
#'
#' @inheritParams sim
#' @param sim_object list with design and simulation parameters
#' 
#' @export
#' 
#' @return data.frame with simulation results
#' 
sim_core <- function(
  sim_object = NULL,
  ode,
  duplicate_t_obs = FALSE,
  t_init = 0,
  lagtime = c(0)
) {
  tmp <- ode(
    A = sim_object$A_init,
    design = sim_object$design,
    par = sim_object$p,
    iov_bins = sim_object$iov_bins,
    lagtime = lagtime,
    step_size = sim_object$int_step_size
  )
  out <- data.frame(t = tmp$time, y = tmp$obs, obs_type = tmp$obs_type)
  if(duplicate_t_obs) {
    # use match to ensure that duplicates in t_obs is possible
    # make sure rounding level is same! We use 6 digits in sim.R
    out <- out[match(paste(sim_object$obs_type, round(sim_object$t_obs, 6), sep="_"),
                     paste(out$obs_type, round(out$t, 6), sep = "_")),]
  } else {
    out <- out[!duplicated(out$t) & out$t %in% sim_object$t_obs,]
  }
  out$t <- out$t - t_init
  return(out)
}
