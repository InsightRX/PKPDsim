#' Dose regimen for sim_ode
#'
#' Create a dosing regimen for use with sim_ode
#' @param amt dosing amount, either a single value (which will repeated for multiple doses), or a vector with doses for each administration
#' @param interval dosing interval (requires n as argument)
#' @param n number of doses (requires interval as argument)
#' @param times vector describing dosing times. Overrides specified times using interval and n arguments
#' @param type either "infusion" or "bolus" (default)
#' @param t_inf infusion time (if type==infusion)
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
                    interval = 12,
                    n = 3,
                    times = NULL,
                    type = NULL,
                    t_inf = NULL,
                    t_lag = NULL,
                    cmt = NULL,
                    checks = TRUE,
                    ss = FALSE,
                    n_ss = NULL,
                    first_dose_time = lubridate::now()) {
  reg <- structure(list(amt = amt,
                        interval = interval,
                        n = n,
                        type = type,
                        t_inf = t_inf), class = "regimen")
  if(checks) {
    if(any(reg$amt < 0)) {
      reg$amt[reg$amt < 0] <- 0
      warning("Some doses were < 0, setting to 0.")
    }
    if (is.null(type) || length(type) == 0 || !(type %in% c("bolus", "oral", "infusion"))) {
      if(!is.null(t_inf)) {
        type <- "infusion"
      } else {
        message("Type argument should be one of 'bolus', 'oral', or 'infusion'. Assuming bolus for all doses.")
        type <- "bolus"
      }
    }
    if (is.null(times) && is.null(interval)) {
      stop("Dose times or dosing interval has to be specified.")
    }
    if (is.null(times) && !is.null(interval) && is.null(n)) {
      stop("The number of doses (n) must be specified in the regimen object.")
    }
    if(is.null(t_inf)) {
      reg$t_inf = 1
    }
    if(any(reg$t_inf == 0)) {
      message("Infusion time cannot be zero, changing to 1 minute instead.")
      reg$t_inf[reg$t_inf == 0] <- 1/60
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
      type = type,
      checks = FALSE,
      ss = FALSE)
    reg$ss_regimen <- pre_reg
  }
  if(is.null(times)) {
    reg$dose_times <- c(0:(n-1)) * interval
  } else {
    reg$dose_times <- times
  }
  reg$n <- length(reg$dose_times)
  if (length(reg$amt) != length(reg$dose_times)) {
    reg$dose_amts <- rep(reg$amt[1], length(reg$dose_times))
  } else {
    reg$dose_amts <- reg$amt
  }
  if(length(reg$t_inf) != length(reg$dose_times)) {
    reg$t_inf <- rep(reg$t_inf[1], length(reg$dose_times))
  }
  if(length(reg$type) != length(reg$dose_times)) {
    reg$type <- rep(type[1], length(reg$dose_times))
  } else {
    reg$type <- type
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
  reg$first_dose_time <- first_dose_time
  return(reg)
}
