#' Create ADVAN-style dataset
#'
#' @param regimen PKPDsim regimen
#' @param parameters list of parameters
#' @param cmts number of compartments, minimum is 1. Default is 5, which is enough for most linear PK models. It is OK to have more compartments available than are actually being used.
#' @param t_obs add observation timepoints to dataset
#'
#' @export
advan_create_data <- function(regimen, parameters, cmts = 5, t_obs = NULL) {
  data <- data.frame(
    ID = 1,
    TIME = regimen$dose_times,
    AMT = dose, EVID = 1, DV = 0,
    RATE = 0,
    TYPE = ifelse(regimen$type == "infusion", 1, 0))
  if(!is.null(t_obs)) {
    obs <- tail(data, 1)
    obs$AMT <- 0
    obs$RATE <- 0
    obs$EVID <- 0
    obs$TYPE <- 0
    for(i in seq(t_obs)) {
      obs$TIME <- t_obs[i]
      data <- rbind(data, obs)
    }
    data <- data[order(data$TIME, -data$EVID),]
  }
  for(i in 1:cmts) {
    data[[paste0("A", i)]] <- 0
  }
  for(key in names(parameters)) {
    data[[key]] <- parameters[[key]]
  }
  inf_idx <- regimen$type == "infusion"
  if(any(inf_idx)) {
    data_idx <- data$TYPE == 1
    data$RATE[data_idx] <- data$AMT[data_idx] / regimen$t_inf[inf_idx]
    data <- advan_process_infusion_doses(data)
  }
  data$TYPE <- NULL
  return(data)
}
