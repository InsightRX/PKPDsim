#' Convert PKPDsim regimen to NONMEM table (doses only)
#'
#' @param reg `PKPDsim` regimen, created using `new_regimen()` function
#' @param dose_cmt dosing compartment, if not specified in `reg` object
#' @param n_ind repeat for `n_ind` subjects
#'
#' @export
regimen_to_nm <- function(reg = NULL, dose_cmt = 1, n_ind = 1) {
  if(is.null(regimen) || ! "regimen" %in% class(reg)) {
    error("No regimen or invalid regimen object supplied.")
  }
  dat <- data.frame(cbind(
    ID = rep(1:n_ind, each = length(reg_vanco$dose_times)),
    TIME = reg_vanco$dose_times,
    CMT = dose_cmt,
    DV = 0,
    AMT = reg_vanco$dose_amts,
    EVID = 1,
    MDV = 1))
  if(any(reg$type == "infusion")) {
    dat$RATE <- reg$dose_amts / reg$t_inf
  }
  return(dat)
}
