#' New covariate
#'
#' Describe data for a covariate, either fixed or time-variant
#' @param value a numeric vector
#' @param times NULL for time-invariant covariate or a numeric vector specifying the update times for the covariate
#' @param implementation for time-varying covariates either 'LOCF' (last observation carried forward) or 'interpolate' (default)
#' @param interpolation_join_limit for `interpolate` option, if covariate timepoints are spaced too close together, the ODE solver sometimes chokes. This argument sets a lower limit on the space between timepoints. It will create average values on joint timepoints instead. If undesired set to NULL or 0.
#' @param unit specify covariate unit (optional, for documentation purposes only)
#' @param remove_negative_times `TRUE`` or `FALSE`
#' @param verbose verbosity
#' @export
new_covariate <- function(
  value=NULL,
  times=NULL,
  implementation = "interpolate",
  unit = NULL,
  interpolation_join_limit = 1,
  remove_negative_times = TRUE,
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
  if(remove_negative_times) {
    if(any(new_times < 0)) { # extend to time zero if any observation is < 0
      if(! (0 %in% new_times)) {
        # add zero time, and remove all t < 0
        if(any(new_times > 0)) { # add the interpolated obs before and after 0 as t=0
          if(implementation == 'interpolate') {
            new_times <- c(new_times, 0)
            y1 <- utils::tail(new_values[new_times < 0],1)
            y2 <- utils::head(new_values[new_times > 0],1)
            t1 <- utils::tail(new_times[new_times < 0],1)
            t2 <- utils::head(new_times[new_times > 0],1)
            grad <-  (y2-y1) / (t2-t1)
            new_values <- c(new_values, y1 + grad * (0-t1))
          } else { # add the last obs before 0 as t=0
            new_times <- c(new_times, 0)
            new_values <- c(new_values, utils::tail(new_values[new_times < 0], 1))
          }
        } else {
          new_times <- 0
          new_values <- utils::tail(new_values,1)
        }
      }
    }
    new_values <- new_values[new_times >= 0]
    new_times <- new_times[new_times >= 0]
    srt <- order(new_times)
    new_times <- new_times[srt]
    new_values <- new_values[srt]
  }
  if(min(new_times) > 0) { # extend to time zero if first observation is > 0
    if(new_times[1] > interpolation_join_limit) {
      # add observation at t=0
      new_times <- c(0, new_times)
      new_values <- c(new_values[1], new_values)
    } else {
      # assume observation was at t=0
      new_times <- c(0, new_times[-1])
      message("Note: time for first covariate measurement set to 0.")
    }
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
