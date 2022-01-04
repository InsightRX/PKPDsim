#' Internal function to parse the raw output from ADVAN-style functions
#'
#' @param data simulation output data
#' @param cmts number of compartments
#' @param t_obs observation times
#' @param extra_t_obs leave extra added dose times in dataset?
#' @param regimen PKPDsim regimen
#'
#' @export
#' @return Data frame containing parsed simulation data
advan_parse_output <- function(
  data,
  cmts = 1,
  t_obs,
  extra_t_obs = TRUE,
  regimen
  ) {

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
