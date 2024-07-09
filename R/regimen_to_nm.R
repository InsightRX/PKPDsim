#' Convert PKPDsim regimen to NONMEM table (doses only)
#'
#' @param reg `PKPDsim` regimen, created using `new_regimen()` function
#' @param dose_cmt dosing compartment, if not specified in `reg` object
#' @param n_ind repeat for `n_ind` subjects
#' @param t_obs add observation time(s)
#' @param obs_cmt observation compartment for added observation time(s)
#' @param bioav bioavailability (numeric vector, can not be a parameter)
#'
#' @export
#' @return Data frame containing doses
regimen_to_nm <- function(
  reg = NULL,
  dose_cmt = 1,
  n_ind = 1,
  t_obs = NULL,
  obs_cmt = 1,
  bioav = NULL
) {
  if(is.null(reg) || ! "regimen" %in% class(reg)) {
    stop("No regimen or invalid regimen object supplied.")
  }
  dat <- data.frame(cbind(
    ID = rep(1:n_ind, each = length(reg$dose_times)),
    TIME = reg$dose_times,
    CMT = dose_cmt,
    DV = 0,
    AMT = reg$dose_amts,
    EVID = 1,
    MDV = 1)
  )
  has_t_inf <- isTRUE(any(reg$t_inf > 0))
  if(has_t_inf) {
    dat$RATE <- reg$dose_amts / reg$t_inf
    dat$RATE[reg$t_inf == 0] <- 0           # rate of zero indicates bolus
    if(!is.null(bioav[dose_cmt])) {
      suppressWarnings(
        bioav_dose <- as.numeric(bioav[dose_cmt])
      )
      if(any(is.na(bioav_dose))) {
        if(any(is.na(bioav_dose) & reg$t_inf > 0)) { # only warn when it actually concerns an infusion
          warning("For compartments where bioavailability is specified as model parameter and not as a number, any infusion rates are not corrected for bioavailability.")
        }
        bioav_dose[is.na(bioav_dose)] <- 1
      }
      dat$RATE <- dat$RATE * bioav_dose
      message("Recalculating infusion rates to reflect bioavailability for infusion.")
    }
  }
  if(!is.null(t_obs)) {
    obs <- data.frame(
      ID = rep(1:n_ind, each = length(t_obs)),
      TIME = rep(t_obs, n_ind))
    obs$CMT <- obs_cmt
    obs$DV <- 0
    obs$AMT <- 0
    obs$EVID <- 0
    obs$MDV <- 0
    if(has_t_inf) {
      obs$RATE <- 0
    }
    dat <- rbind(dat, obs)
    dat <- dat[order(dat$ID, dat$TIME, -dat$EVID, dat$CMT),]
  }
  return(dat)
}
