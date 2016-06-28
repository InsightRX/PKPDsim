#' Join two dosing regimens
#'
#' @param regimen1 first regimen
#' @param regimen2 second regimen
#' @param interval interval between regimen1 and regimen2 (if dose_update not specified)
#' @param dose_update dose number at which to override regimen1 with regimen 2 (if interval not specified)
#' @export
join_regimen <- function(
  regimen1 = NULL,
  regimen2 = NULL,
  interval = NULL,
  dose_update = NULL) {
  if(is.null(regimen1) && is.null(regimen2)) {
    stop("please specify two intervals to this function.")
  } else {
    if(is.null(regimen1)) { # if one of the intervals is NULL, just return the other
      return(regimen2)
    }
    if(is.null(regimen2)) {
      return(regimen1)
    }
    if(is.null(interval) && is.null(dose_update)) {
      stop("either interval or dose_update have to be specified as arguments")
    }
    if(!is.null(dose_update) && dose_update < 1) {
      dose_update <- NULL
    }
    if(!is.null(dose_update)) {
      if(dose_update > 1) {
        if(dose_update <= length(regimen1$dose_times)) {
          t <- c(regimen1$dose_times[1:(dose_update-1)], regimen2$dose_times + regimen1$dose_times[dose_update])
          amt <- c(regimen1$dose_amts[1:(dose_update-1)], regimen2$dose_amts)
          t_inf <- c(regimen1$t_inf[1:(dose_update-1)], regimen2$t_inf)
        } else { # just add to regimen
          t <- c(regimen1$dose_times, regimen2$dose_times + tail(regimen1$dose_times,1) + interval)
          amt <- c(regimen1$dose_amts, regimen2$dose_amts)
          t_inf <- c(regimen1$t_inf, regimen2$t_inf)
        }
      } else {
        t <- c(regimen2$dose_times + regimen1$dose_times[dose_update])
        amt <- c(regimen2$dose_amts)
        t_inf <- c(regimen2$t_inf)
      }
      joint <- new_regimen(amt = amt, times = t, t_inf = t_inf, type = regimen1$type)
      return(joint)
    }
    if(!is.null(interval)) {
      joint <- new_regimen(
        amt = c(regimen1$dose_amts, regimen2$dose_amts),
        times = c(regimen1$dose_times, regimen2$dose_times + tail(regimen1$dose_times,1) + interval),
        type = "infusion",
        t_inf = c(regimen1$t_inf, regimen2$t_inf)
      )
      return(joint)
    } else {
      stop("Please provide the interval between the two dose regimens (`interval`), or else the dose number from which to update (`dose_update`)")
    }
  }
}
