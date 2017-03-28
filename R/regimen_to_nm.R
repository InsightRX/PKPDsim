#' Convert PKPDsim regimen to NONMEM table (doses only)
#'
#' @param reg `PKPDsim` regimen, created using `new_regimen()` function
#' @param dose_cmt dosing compartment, if not specified in `reg` object
#' @param n_ind repeat for `n_ind` subjects
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
    obs <- nm_tab %>% filter(!duplicated(ID))
    obs <- data.frame(cbind(
      ID = obs$ID,
      TIME = rep(t_obs, each = length(obs$ID)),
      obs[,3:length(obs)]))
    obs <- obs %>% dplyr::mutate(EVID = 0, MDV = 0, AMT = 0, CMT = obs_comp)
    if("RATE" %in% colnames(obs)) {
      obs$RATE <- 0
    }
    dat <- bind_rows(dat, obs) %>% arrange(ID, TIME, -EVID, CMT)
  }
  return(dat)
}
