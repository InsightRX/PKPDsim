#' Join two dosing regimens
#'
#' @param regimen1 first regimen
#' @param regimen2 second regimen
#' @param interval interval between regimen1 and regimen2 (if dose_update not specified)
#' @param dose_update dose number at which to override regimen1 with regimen 2 (if interval not specified)
#' @param t_dose_update dose time from which to update regimen
#' @param continuous for joining continuous infusions
#' @export
#' @return Joined regimen
join_regimen <- function(
  regimen1 = NULL,
  regimen2 = NULL,
  interval = NULL,
  dose_update = NULL,
  t_dose_update = NULL,
  continuous = FALSE) {
  if(is.null(regimen1) && is.null(regimen2)) {
    stop("please specify two intervals to this function.")
  }
  if(is.null(regimen1)) { # if one of the intervals is NULL, just return the other
    return(regimen2)
  }
  if(is.null(regimen2)) {
    return(regimen1)
  }
  if(!is.null(dose_update) && dose_update < 1) {
    dose_update <- NULL
  }
  if(is.null(interval) && isFALSE(dose_update <= length(regimen1$dose_times)) && is.null(t_dose_update)) {
    stop("either interval or dose_update have to be specified as arguments")
  }
  if(!is.null(t_dose_update)) { # from a specific time
    keep <- which(regimen1$dose_times < t_dose_update)
    regimen1$dose_times <- c(regimen1$dose_times[keep], regimen2$dose_times + t_dose_update)
    regimen1$dose_amts <- c(regimen1$dose_amts[keep], regimen2$dose_amts)
    ## when we join, we don't want the last infusion to overlap with
    ## the 1st one from the 2nd regimen, and should run until then.
    if(isTRUE(length(keep) > 0 && regimen1$t_inf[max(keep)] > (t_dose_update - regimen1$dose_times[max(keep)]))) {
      planned_t_inf <- regimen1$t_inf[max(keep)]
      new_t_inf <- t_dose_update - max(regimen1$dose_times[keep])
      regimen1$t_inf[max(keep)] <- new_t_inf
      regimen1$dose_amts[max(keep)] <- regimen1$dose_amts[max(keep)] * (new_t_inf/planned_t_inf)
    }
    regimen1$t_inf <- c(regimen1$t_inf[keep], regimen2$t_inf)
    regimen1$interval <- regimen2$interval
    regimen1$type <- c(regimen1$type[keep], regimen2$type)
    return(regimen1)
  }

  if(!is.null(dose_update)) {
    if(dose_update > 1) {
      if(dose_update <= length(regimen1$dose_times)) {
        t <- c(regimen1$dose_times[1:(dose_update-1)], regimen2$dose_times + regimen1$dose_times[dose_update])
        amt <- c(regimen1$dose_amts[1:(dose_update-1)], regimen2$dose_amts)
        types <- c(regimen1$type[1:(dose_update-1)], regimen2$type)
        t_inf <- c(regimen1$t_inf[1:(dose_update-1)], regimen2$t_inf)
      } else { # just add to regimen
        t <- c(regimen1$dose_times, regimen2$dose_times + utils::tail(regimen1$dose_times,1) + interval)
        amt <- c(regimen1$dose_amts, regimen2$dose_amts)
        types <- c(regimen1$type, regimen2$type)
        t_inf <- c(regimen1$t_inf, regimen2$t_inf)
      }
    } else {
      t <- c(regimen2$dose_times + regimen1$dose_times[dose_update])
      amt <- c(regimen2$dose_amts)
      types <- c(regimen2$type)
      t_inf <- c(regimen2$t_inf)
    }
    joint <- new_regimen(amt = amt, times = t, t_inf = t_inf, type = types, interval = interval)
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
    ## when we join, we don't want the last infusion to overlap with
    ## the 1st one from the 2nd regimen, and should run until then.
    if(isTRUE(tail(regimen1$t_inf,1) > interval)) {
      planned_t_inf <- tail(regimen1$t_inf,1)
      regimen1$t_inf[length(regimen1$t_inf)] <- interval
      regimen1$dose_amts[length(regimen1$t_inf)] <- regimen1$dose_amts[length(regimen1$t_inf)] * interval / planned_t_inf
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
