#' Only core function of the simulation function, always just returns observations.
#' Mostly useful for estimations / optimal design. Has no checks (for speed)!
#'
#' @param sim_object list with design and simulation parameters
#' @param ode ode
#' @export
sim_core <- function(sim_object = NULL, ode) {
  tmp <- ode(sim_object$A_init,
             sim_object$design,
             sim_object$p,
             sim_object$int_step_size)
  out <- data.frame(t = tmp$time, y = tmp$obs)
  return(out[out$t %in% sim_object$t_obs,])
}
