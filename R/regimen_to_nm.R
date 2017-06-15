#' Convert PKPDsim regimen to NONMEM table (doses only)
#'
#' @param reg `PKPDsim` regimen, created using `new_regimen()` function
#' @param dose_cmt dosing compartment, if not specified in `reg` object
#' @param n_ind repeat for `n_ind` subjects
#' @param t_obs add observation time(s)
#' @param obs_cmt observation compartment for added observation time(s)
#'
#' @export
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
      TIME = rep(t_obs, each = length(1:n_ind)),
      DV = 0)
    obs <- obs %>% dplyr::mutate("EVID" = 0, "MDV" = 0, "AMT" = 0, "CMT" = obs_cmt)
    if("RATE" %in% colnames(dat)) {
      obs$RATE <- 0
    }
    dat <- dplyr::bind_rows(dat, obs)
    dat <- dat[order(dat$ID, dat$TIME, -dat$EVID, dat$CMT),] # not using arrange due to package check error (NOTE no visible binding etc.)
  }
  return(dat)
}
