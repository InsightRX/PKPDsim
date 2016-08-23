#' New covariate
#'
#' Describe data for a covariate, either fixed or time-variant
#' @param value a numeric vector
#' @param times NULL for time-invariant covariate or a numeric vector specifying the update times for the covariate
#' @param implementation for time-varying covariates either 'LOCF' (last observation carried forward) or 'interpolate' (default)
#' @param unit specify covariate unit (optional, for documentation purposes only)
#' @export
new_covariate <- function(value=NULL, times=NULL, implementation = "interpolate", unit = NULL) {
  if(is.null(value)) {
    stop("Covariate value required!")
  }
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
              implementation = implementation,
              unit = unit)
  class(cov) <- c(class(cov), "covariate")
  return(cov)
}
