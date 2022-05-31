#' Dose regimen for sim_ode
#'
#' Create a dosing regimen for use with sim_ode
#' @param amt dosing amount, either a single value (which will repeated for multiple doses), or a vector with doses for each administration
#' @param interval dosing interval (requires n as argument)
#' @param n number of doses (requires interval as argument)
#' @param times vector describing dosing times. Overrides specified times using interval and n arguments
#' @param type either "infusion", "bolus", "oral", "sc" (subcutaneous), or "im" (intramuscular).
#' @param t_inf infusion time (if `type`==`infusion`)
#' @param rate infusion rate (if `type`==`infusion`). `NULL` by default. If specified, overrides `t_inf`
#' @param t_lag lag time (can be applied to any dose type, not only oral). Will just be added to `times`
#' @param cmt vector of dosing compartments (optional, if NULL will dosing compartment defined in model will be used)
#' @param first_dose_time datetime stamp of first dose (of class `POSIXct`). Default is current date time.
#' @param checks input checks. Remove to increase speed (e.g. for population-level estimation or optimal design)
#' @param ss steady state? boolean value whether to simulate out to steady state first (steady state will be based on specified `amt` and `interval`, `times` will be ignored).
#' @param n_ss how many doses to simulate before assumed steady state. Default is 4 * 24 / `interval`.
#' @return a list containing calculated VPC information, and a ggplot2 object
#' @export
#' @seealso \link{sim_ode}
#' @examples
#'
#'r1 <- new_regimen(amt=50, interval=12, n=20) # dose 50mg, q12hrs for 10 days
#'r2 <- new_regimen(amt=50, times=c(0:19)*12)  # same, but using explicit times
#'r3 <- new_regimen(amt=c(rep(100,4), rep(50,16)), times=c(0:19)*12)  # first 4 doses higher dose

new_regimen <- function(
    amt = 100,
    interval = NULL,
    n = 3,
    times = NULL,
    type = NULL,
    t_inf = NULL,
    rate = NULL,
    t_lag = NULL,
    cmt = NULL,
    checks = TRUE,
    ss = FALSE,
    n_ss = NULL,
    first_dose_time = now_utc()) {

  reg <- structure(list(amt = amt,
                        interval = interval,
                        n = n,
                        type = type,
                        t_inf = t_inf), class = c("regimen", "list"))
  if(checks) {
    if(any(reg$amt < 0)) {
      reg$amt[reg$amt < 0] <- 0
      warning("Some doses were < 0, setting to 0.")
    }
    if(is.null(reg$type)) {
      if(!is.null(reg$t_inf)) {
        reg$type <- rep("infusion", length(reg$t_inf))
        reg$type[reg$t_inf == 0] <- "bolus"
        if(any(reg$t_inf > 0)) {
          warning("Please specify regimen `type` explicitly (either 'infusion', 'bolus', 'oral', 'sc' or 'im'). Will assume `type='infusion'` for doses with `t_inf` > 0.")
        }
      } else {
        reg$type <- "bolus"
      }
    }
    if(!is.null(reg$type) && (any(is.null(reg$type)) || any(is.na(reg$type)) || any(length(reg$type) == 0) || !(all(reg$type %in% c("bolus", "oral", "infusion", "sc", "im"))))) {
      if(!is.null(t_inf) || !is.null(rate)) {
        reg$type <- "infusion" # assume all infusions
      } else {
        message("Type argument should be one of 'bolus', infusion', 'oral', 'sc' or 'im'. Assuming bolus for all doses.")
        reg$type <- "bolus"
      }
    }
    if (is.null(times) && is.null(interval)) {
      stop("Dose times or dosing interval has to be specified.")
    }
    if (is.null(times) && !is.null(interval) && is.null(n)) {
      stop("The number of doses (n) must be specified in the regimen object.")
    }
    if(any(type == "infusion") && (is.null(t_inf) || length(t_inf) == 0)) {
      reg$t_inf = 1
    } else if (any(is.na(t_inf))) {
      t_inf[is.na(t_inf)] <- 1
    }
  }
  if(ss) {
    if(is.null(amt) || is.null(interval)) {
      stop("'amt' and 'interval' are required when steady state is requested.")
    }
    if(is.null(n_ss) || n_ss <= 0) {
      n_ss <- 5 * 24/interval
    }
    pre_reg <- new_regimen(
      amt = amt,
      interval = interval,
      n = n_ss,
      cmt = cmt,
      t_inf = t_inf,
      type = reg$type,
      checks = FALSE,
      ss = FALSE)
    reg$ss_regimen <- pre_reg
  }
  if(is.null(times)) {
    reg$dose_times <- c(0:max(0, n-1)) * interval
  } else {
    reg$dose_times <- times
    if(length(reg$dose_times) > 1) {
      reg$interval <- diff(utils::tail(reg$dose_times, 2))
    } else {
      reg$interval <- 24
    }
  }
  reg$n <- length(reg$dose_times)
  if (length(reg$amt) != length(reg$dose_times)) {
    reg$dose_amts <- rep(reg$amt[1], reg$n)
  } else {
    reg$dose_amts <- reg$amt
  }
  reg$dose_amts <- as.num(reg$dose_amts)
  if(!is.null(rate) && sum(rate != 0) > 0) {
    if(length(rate) != length(reg$dose_times)) {
      rate <- rep(rate, reg$n)
    }
    reg$t_inf[rate != 0] <- reg$dose_amts[rate!=0] / rate[rate!=0]
  }
  if(length(reg$t_inf) != length(reg$dose_times)) {
    reg$t_inf <- rep(reg$t_inf[1], length(reg$dose_times))
  }
  if(length(reg$type) != length(reg$dose_times)) {
    reg$type <- rep(reg$type[1], length(reg$dose_times))
  }
  if(any(reg$type == "infusion")) {
    if(any(reg$t_inf == 0)) {
      reg$t_inf[reg$t_inf == 0] <- 1/60
      reg$rate[reg$t_inf == 0] <- 60
    }
  }
  if(any(reg$type == "bolus")) {
    reg$t_inf[reg$type == "bolus"] <- 0
    reg$rate[reg$type == "bolus"] <- 0
  }
  if(any(reg$type == "oral")) {
    reg$t_inf[reg$type == "oral"] <- 0
    reg$rate[reg$type == "oral"] <- 0
  }
  if(!is.null(cmt)) {
    if(length(cmt) != length(reg$dose_times)) {
      cmt <- rep(cmt[1], length(reg$dose_times))
    }
    reg$cmt <- cmt
  }
  ## check that all amounts are available, otherwise remove
  if(!is.null(reg$t_inf)) {
    reg$t_inf <- reg$t_inf[!is.na(reg$dose_amts)]
  }
  reg$dose_times <- reg$dose_times[!is.na(reg$dose_amts)]
  reg$dose_amts <- reg$dose_amts[!is.na(reg$dose_amts)]
  reg$amt <- NULL
  reg$first_dose_time <- first_dose_time
  if(!is.null(t_lag)) {
    reg$dose_times <- reg$dose_times + t_lag
  }
  return(reg)
}
