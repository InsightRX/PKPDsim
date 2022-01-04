#' Convert PKPDsim regimen to NONMEM table (doses only)
#'
#' @param reg `PKPDsim` regimen, created using `new_regimen()` function
#' @param dose_cmt dosing compartment, if not specified in `reg` object
#' @param n_ind repeat for `n_ind` subjects
#' @param t_obs add observation time(s)
#' @param obs_cmt observation compartment for added observation time(s)
#'
#' @export
#' @return Data frame containing doses
regimen_to_nm <- function(
  reg = NULL,
  dose_cmt = 1,
  n_ind = 1,
  t_obs = NULL,
  obs_cmt = 1) {
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
    MDV = 1))
  if(any(reg$type == "infusion")) {
    dat$RATE <- reg$dose_amts / reg$t_inf
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
    if(any(reg$type == "infusion")) {
      obs$RATE <- 0
    }
    dat <- rbind(dat, obs)
    dat <- dat[order(dat$ID, dat$TIME, -dat$EVID, dat$CMT),]
  }
  return(dat)
}
