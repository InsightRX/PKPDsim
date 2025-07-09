#' Parse lagtime specified to main sim() function
#' 
#' @inheritParams sim
#' 
#' @returns a vector of character or numeric values
#' 
parse_lagtime <- function(
  lagtime,
  ode
) {
  lagtime_ode <- attr(ode, "lagtime")
  # override from ode if not specified by user and defined in ode
  if(is.null(lagtime) && !is.null(lagtime_ode) && !lagtime_ode %in% c("NULL", "undefined")) {
    lagtime <- lagtime_ode
  }
  if(is.null(lagtime)) {
    lagtime <- 0
  }
  lagtime
}