#' Join two dosing regimens
#'
#' @param regimen1 first regimen
#' @param regimen2 second regimen
#' @param interval interval between regimen1 and regimen2 (if dose_update not specified)
#' @param dose_update dose number at which to override regimen1 with regimen 2 (if interval not specified)
#' @param t_dose_update dose time from which to update regimen
#' @export
join_regimen <- function(
  regimen1 = NULL,
  regimen2 = NULL,
  interval = NULL,
  dose_update = NULL,
  t_dose_update = NULL) {
  if(is.null(regimen1) && is.null(regimen2)) {
    stop("please specify two intervals to this function.")
  } else {
    if(is.null(regimen1)) { # if one of the intervals is NULL, just return the other
      return(regimen2)
    }
    if(is.null(regimen2)) {
      return(regimen1)
    }
    if(is.null(interval) && is.null(dose_update) && is.null(t_dose_update)) {
      stop("either interval or dose_update have to be specified as arguments")
    }
    if(!is.null(t_dose_update)) { # from a specific time
      keep <- (regimen1$dose_times + regimen1$t_inf) < t_dose_update
      regimen1$dose_times <- c(regimen1$dose_times[keep], regimen2$dose_times + t_dose_update)
      regimen1$dose_amts <- c(regimen1$dose_amts[keep], regimen2$dose_amts)
      regimen1$t_inf <- c(regimen1$t_inf[keep], regimen2$t_inf)
      regimen1$interval <- regimen2$interval
      regimen1$type <- c(regimen1$type, regimen2$type)
      return(regimen1)
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
          t <- c(regimen1$dose_times, regimen2$dose_times + utils::tail(regimen1$dose_times,1) + interval)
          amt <- c(regimen1$dose_amts, regimen2$dose_amts)
          t_inf <- c(regimen1$t_inf, regimen2$t_inf)
        }
      } else {
        t <- c(regimen2$dose_times + regimen1$dose_times[dose_update])
        amt <- c(regimen2$dose_amts)
        t_inf <- c(regimen2$t_inf)
      }
      joint <- new_regimen(amt = amt, times = t, t_inf = t_inf, type = regimen1$type, interval = interval)
      if(!is.null(regimen1$ss_regimen)) {
        joint$ss_regimen <- regimen1$ss_regimen
      }
      return(joint)
    }
    if(!is.null(interval)) {
      tmp2 <-  regimen2$dose_times + utils::tail(regimen1$dose_times,1) + interval
      if(tmp2[1] == utils::tail(regimen1$dose_times,1)) {
        tmp2[1] <- utils::tail(regimen1$dose_times,1) + 0.01
        message("Dose for second regimen planned at same time as last dose in 1st regimen. Added 0.01 hrs to administration time of 1st dose of 2nd regimen to avoid simulation issues.")
      }
      joint <- new_regimen(
        amt = c(regimen1$dose_amts, regimen2$dose_amts),
        times = c(regimen1$dose_times, tmp2),
        cmt = c(regimen1$cmt, regimen2$cmt),
        type = c(regimen1$type, regimen2$type),
        t_inf = c(regimen1$t_inf, regimen2$t_inf),
        interval = interval
      )
      if(!is.null(regimen1$ss_regimen)) {
        joint$ss_regimen <- regimen1$ss_regimen
      }
      return(joint)
    } else {
      stop("Please provide the interval between the two dose regimens (`interval`), or else the dose number from which to update (`dose_update`)")
    }
  }
}
