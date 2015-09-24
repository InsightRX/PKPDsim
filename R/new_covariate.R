#' New covariate
#'
#' Describe data for a covariate, either fixed or time-variant
#' @param value a numeric vector
#' @param times NULL for time-invariant covariate or a numeric vector specifying the update times for the covariate
#' @param implementation for time-varying covariates either LOCF or interpolate
#' @export
new_covariate <- function(value=70, times=NULL, implementation = "LOCF") {
  if(is.null(times)) { # time invariant
    times <- c(0)
    value <- value[1]
  }
  if(min(times)>0) { # extend to time zero if first observation is >0
    times <- c(0, times)
    value <- c(value[1], value)
  }
  cov <- list(value = value,
              times = times,
              implementation = "LOCF") # currently only LOCF supported!
  class(cov) <- c(class(cov), "covariate")
  return(cov)
}
