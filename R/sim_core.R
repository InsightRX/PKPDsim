#' Only core function of the simulation function, always just returns observations.
#' Mostly useful for estimations / optimal design. Has no checks (for speed)!
#'
#' @param sim_object list with design and simulation parameters
#' @param ode ode
#' @export
sim_core <- function(sim_object = NULL, ode, t_obs) {
  tmp <- ode(sim_object$A_init,
             sim_object$design,
             sim_object$p,
             sim_object$int_step_size)
  return(tmp$obs[!duplicated(tmp$time) & tmp$time %in% t_obs])
}
