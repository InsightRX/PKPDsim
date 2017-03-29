#' New covariate
#'
#' Describe data for a covariate, either fixed or time-variant
#' @param value a numeric vector
#' @param times NULL for time-invariant covariate or a numeric vector specifying the update times for the covariate
#' @param implementation for time-varying covariates either 'LOCF' (last observation carried forward) or 'interpolate' (default)
#' @param interpolation_join_limit for `interpolate` option, if covariate timepoints are spaced too close together, the ODE solver sometimes chokes. This argument sets a lower limit on the space between timepoints. It will create average values on joint timepoints instead. If undesired set to NULL or 0.
#' @param unit specify covariate unit (optional, for documentation purposes only)
#' @param verbose verbosity
#' @export
new_covariate <- function(
  value=NULL,
  times=NULL,
  implementation = "interpolate",
  unit = NULL,
  interpolation_join_limit = 1,
  verbose = TRUE) {
  if(is.null(value)) {
    stop("Covariate value required!")
  }
  if(is.null(times)) { # time invariant
    times <- c(0)
    value <- value[1]
  }
  srt <- order(times)
  times <- times[srt]
  values <- value[srt]
  if(implementation == "interpolate" && class(values) == "numeric" && !is.null(interpolation_join_limit) && interpolation_join_limit > 0) {
    new_times <- c()
    new_values <- c()
    tmp <- data.frame(cbind(t = times, incl = FALSE))
    msg <- FALSE
    for(i in 1:length(tmp$t)) {
      if(!tmp[i,]$incl) {
        id <- abs(tmp$t - tmp$t[i]) < interpolation_join_limit
        if(sum(id) > 1) {
          msg <- TRUE
        }
        new_times <- c(new_times, mean(times[id]))
        new_values <- c(new_values, mean(values[id]))
        new_values <- c(new_values, mean(values[id]))
        tmp$incl[id] <- TRUE
      }
    }
    if(verbose && msg) {
      message("Note: Some covariate observations were joined to protect against ODE solver issues, see help on `interpolation_join_limit` argument for more info. Important: Units are disregarded in this join, so please preprocess data if units vary!")
    }
  } else {
    new_times <- times
    new_values <- values
  }
  if(min(times)>0) { # extend to time zero if first observation is >0
    new_times <- c(0, new_times)
    new_values <- c(new_values[1], new_values)
  }
  if(length(unit) > length(new_values)) {
    unit <- unit[1:length(new_values)]
  }
  cov <- list(value = new_values,
              times = new_times,
              implementation = implementation,
              unit = unit)
  class(cov) <- c(class(cov), "covariate")
  return(cov)
}
