#' Internal function to parse the raw output from ADVAN-style functions
#'
#' @param data simulation output data
#'
#' @export
advan_parse_output <- function(data, cmts = 1, t_obs, extra_t_obs = TRUE, regimen) {
  cmts <- PKPDsim::ifelse0(cmts, 1)
  out <- data[, c("ID", "TIME", "DV", paste0("A", 1:cmts))]
  names(out) <- c("id", "t", "y", paste0("A", 1:cmts))
  out$obs_type <- 1

  ## Clean up
  if(!extra_t_obs) {
    t_doses <- regimen$dose_times
    idx <- (out$t %in% t_doses) & !duplicated(out$t) # any added extra obs for bolus doses
    out <- out[!idx & out$t %in% t_obs,] # also remove any other rows that were not in t_ob
  }

  return(out)
}
