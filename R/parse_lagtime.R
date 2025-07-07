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
  if(!is.null(attr(ode, "lagtime")) && attr(ode, "lagtime")[1] != "undefined" && attr(ode, "lagtime")[1] != "NULL") {
    if(is.null(lagtime)) { # only override from metadata if not specified by user
      lagtime <- attr(ode, "lagtime")
    }
  }
  if(is.null(lagtime)) {
    lagtime <- c(0)
  }
  lagtime
}