#' Simulate ODE and create a Shiny app
#'
#' Creates a VPC plot from observed and simulation data
#' @param ode function describing the ODE system
#' @param dde function describing the DDE system (not implemented yet)
#' @param name of function describing the ODE system (as string)
#' @param parameters model parameters
#' @param omega vector describing the lower-diagonal of the between-subject variability matrix
#' @param omega_type exponential or normal
#' @param n_ind number of individuals to simulate
#' @param regimen a regimen object created using the regimen() function
#' @param A_init vector with the initial state of the ODE system
#' @param t_obs vector of observation times, only output these values
#' @param obs_step_size the step size between the observations
#' @param int_step_size the step size for the numerical integrator
#' @param t_max maximum simulation time, if not specified will pick the end of the regimen as maximum
#' @param shiny_folder folder in which to run Shiny app
#' @return a list containing calculated VPC information, and a ggplot2 object
#' @export
#' @seealso \link{sim_ode}

sim_ode_shiny <- function(name = "",
                          ode = NULL,
                          dde = NULL,
                          parameters = list(),
                          t_obs = NULL,
                          omega = NULL,
                          omega_type = "exponential",
                          n_ind = 1,
                          regimen = NULL,
                          A_init = NULL,
                          obs_step_size = 1,
                          int_step_size = 0.5,
                          t_max = NULL,
                          shiny_folder = "~/shiny-pkpd") {
  if(!file.exists(shiny_folder)) {
    dir.create(shiny_folder)
  } else {
    unlink(paste0(shiny_folder,"/", c("ui.R", "server.R", "ode.rds", "parameters.rds", "misc.rds", "regimen.rds")))
  }
  file.copy(system.file("ui.R", package = "PKPDsim"), paste0(shiny_folder, "/ui.R"))
  file.copy(system.file("server.R", package = "PKPDsim"), paste0(shiny_folder, "/server.R"))
  if(!file.exists(paste0(shiny_folder,"/www"))) {
    dir.create(paste0(shiny_folder,"/www"))
  }
  file.copy(system.file("style.css", package = "PKPDsim"), paste0(shiny_folder, "/www/style.css"))
  saveRDS(parameters, file=paste0(shiny_folder, "/parameters.rds"))
  saveRDS(regimen, file=paste0(shiny_folder, "/regimen.rds"))
  saveRDS(list(ode = ode,
               dde = dde,
               t_obs = t_obs,
               obs_step_size = obs_step_size,
               int_step_size = int_step_size,
               omega = omega,
               omega_type = omega_type,
               n_ind = n_ind,
               A_init = A_init,
               t_max = t_max),
          paste0(shiny_folder, "/misc.rds"))
  runApp(shiny_folder)
}
