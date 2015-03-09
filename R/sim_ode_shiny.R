#' Simulate ODE and create a Shiny app
#'
#' Creates a VPC plot from observed and simulation data
#' @param ode function describing the ODE system
#' @param parameters
#' @param omega vector describing the lower-diagonal of the between-subject variability matrix
#' @param omega_type exponential or normal
#' @param n_ind number of individuals to simulate
#' @param regimen a regimen object created using the regimen() function
#' @param A_init vector with the initial state of the ODE system
#' @param step_size the step size between the observations (NOT the step size of the differential equation solver)
#' @param tmax maximum simulation time, if not specified will pick the end of the regimen as maximum
#' @param output vector specifying which compartment numbers to output
#' @return a list containing calculated VPC information, and a ggplot2 object
#' @export
#' @seealso \link{sim_ode}
#' @examples
#'
#'library(PKPDsim)
#'p <- list(CL = 38.48,
#'          V  = 7.4,
#'          Q2 = 7.844,
#'          V2 = 5.19,
#'          Q3 = 9.324,
#'          V3 = 111)
#'
#'omega <- c(0.3,       # IIV CL
#'           0.1, 0.3)  # IIV V
#'
#'sim_ode_shiny(ode = pk_3cmt_iv,
#'              omega = omega,
#'              par = p)

sim_ode_shiny <- function(name = "",
                          ode = function() {} ,
                          parameters = list(),
                          omega = NULL,
                          omega_type = "exponential",
                          n_ind = 1,
                          regimen = NULL,
                          A_init = NULL,
                          step_size = 1,
                          tmax = NULL,
                          shiny_folder = "~/shiny-pkpd") {
  if(!file.exists(shiny_folder)) {
    dir.create(shiny_folder)
  } else {
    unlink(paste0(shiny_folder,"/", c("ui.R", "server.R", "ode.rds", "parameters.rds", "misc.rds", "regimen.rds")))
  }
  writeLines(ui_text, paste0(shiny_folder, "/ui.R"))
  writeLines(paste0('setwd("',shiny_folder,'")', server_text), paste0(shiny_folder, "/server.R"))
  saveRDS(ode, file=paste0(shiny_folder, "/ode.rds"))
  saveRDS(parameters, file=paste0(shiny_folder, "/parameters.rds"))
  saveRDS(regimen, file=paste0(shiny_folder, "/regimen.rds"))
  saveRDS(list(omega = omega, omega_type = omega_type, n_ind = n_ind, A_init = A_init, step_size = step_size, tmax = tmax),
          paste0(shiny_folder, "/misc.rds"))
  runApp(shiny_folder)
}
