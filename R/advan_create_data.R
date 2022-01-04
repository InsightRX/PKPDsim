#' Create ADVAN-style dataset
#'
#' @param regimen PKPDsim regimen
#' @param parameters list of parameters
#' @param cmts number of compartments, minimum is 1. Default is 5, which is enough for most linear PK models. It is OK to have more compartments available than are actually being used.
#' @param t_obs add observation timepoints to dataset
#' @param covariates covariate list
#' @param covariate_model covariate model equations, written in C
#'
#' @export
#' @return Data frame of ADVAN-style data
advan_create_data <- function(
  regimen,
  parameters,
  cmts = 5,
  t_obs = NULL,
  covariates = NULL,
  covariate_model = NULL) {

  ## Set up basic structure
  data <- data.frame(
    ID = 1,
    TIME = regimen$dose_times,
    AMT = regimen$dose_amts,
    EVID = 1, DV = 0,
    RATE = 0,
    TYPE = ifelse(regimen$type == "infusion", 1, 0))

  ## Add observation data points.
  if(!is.null(t_obs)) {
    obs <- utils::tail(data, 1)
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

  ## Add Compartments
  for(i in 1:cmts) {
    data[[paste0("A", i)]] <- 0
  }
  data$AUC <- 0

  ## Add parameters
  for(key in names(parameters)) {
    data[[key]] <- parameters[[key]]
  }

  ## Parse infusions
  inf_idx <- regimen$type == "infusion"
  if(any(inf_idx)) {
    data_idx <- data$TYPE == 1
    data$RATE[data_idx] <- data$AMT[data_idx] / regimen$t_inf[inf_idx]
    data <- advan_process_infusion_doses(data)
  }
  data$TYPE <- NULL

  ## Add covariates to dataset (simple version)
  for(key in names(covariates)) {
    data[[key]] <- covariates[[key]]$value[1]
  }

  ## Apply covariate model (specified as code) to PK parameters
  ## Could technically also be just a transormation of parameters, it will just
  ## run the specified code on the dataset.
  if(!is.null(covariates) && !is.null(covariate_model)) {
    data <- within(data, eval(parse(text = covariate_model)))
  }

  return(data)
}
