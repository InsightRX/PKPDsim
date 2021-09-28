#' Simulate ODE and create a Shiny app
#'
#' @param ... arguments passed to PKPDsimShiny::sim_ode_shiny()
#' @export
#' @seealso \link{sim_ode}

sim_ode_shiny <- function(...) {
#   if("PKPDsimShiny" %in% rownames(installed.packages())) {
#     PKPDsimShiny::sim_ode_shiny(...)
#   } else {
    message("Sorry, the sim_ode_shiny function is not available in the PKPDsim library anymore, but has its own package! Please install the package 'PKPDsimShiny': devtools::install_github('ronkeizer/PKPDsimShiny'), load the library after you load PKPDsim, and rerun sim_ode_shiny().")
  # }
}
