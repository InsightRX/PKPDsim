#' Parse lagtime specified to main sim() function
#' 
#' @inheritParams sim
#' 
#' @returns a vector of character or numeric values
#' 
parse_lagtime <- function(
  lagtime,
  ode,
  parameters
) {
  lagtime_ode <- attr(ode, "lagtime")
  # override from ode if not specified by user and defined in ode
  if(is.null(lagtime) && !is.null(lagtime_ode) && lagtime_ode[1] != "NULL" && lagtime_ode[1] != "undefined") {
    lagtime <- lagtime_ode
  }
  if(is.null(lagtime)) {
    lagtime <- 0
  }
  if(inherits(lagtime, "character")) {
    idx <- grep("[a-zA-Z]", lagtime) # only pick character names, not "0"
    if(! all(lagtime[idx] %in% names(parameters))) {
      warning("Lagtime parameter(s) not found. Please check model and parameters.")
    }
  } 
  lagtime
}