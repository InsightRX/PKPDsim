#' Only core function of the simulation function, always just returns observations.
#' Mostly useful for estimations / optimal design. Has no checks (for speed)!
#'
#' @param sim_object list with design and simulation parameters
#' @param ode ode
#' @param duplicate_t_obs allow duplicate t_obs in output? E.g. for optimal design calculations when t_obs = c(0,1,2,2,3). Default is FALSE.
#' @export
sim_core <- function(sim_object = NULL, ode, duplicate_t_obs = FALSE) {
  tmp <- ode(sim_object$A_init,
             sim_object$design,
             sim_object$p,
             sim_object$int_step_size)
  out <- data.frame(t = tmp$time, y = tmp$obs)
  if(duplicate_t_obs) {
    return(out[match(sim_object$t_obs, out$t),]) # use match to ensure that duplicates in t_obs is possible
  } else {
    return(out[!duplicated(out$t) & out$t %in% sim_object$t_obs,])
  }
}
