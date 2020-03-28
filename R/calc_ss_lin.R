#' Returns the state of a linear PK system at steady state (trough)
#'
#' Basically it performs a PK simulation using algebraic equations instead
#' of ODEs to steady state (n=45 days, increased if needed). This is much
#' faster than doing the same using ODEs.
#' 
#' The function needs to be made a bit more generic, it focused now
#' on the TwoCompOral model.
#' 
#' It can also be used for models with transit compartments, however, 
#' the assumption is made that at the end of the dosing interval the 
#' amount in the transit compartments is negligible (0).
#'
#' @param dose dose
#' @param interval interval
#' @param model PKPDsim model
#' @param parameters parameters list
#' @param covariates covariates list
#' @param map list for remapping parameters
#' @param n_days number of days at which to assume steady state. Default is 45.
#' @param n_transit_compartments number of transit compartments, will insert n compartments between the first (dose) compartment and the second (central) compartment.
#' @param auc add (empty) AUC compartment at end of state vector?
#' 
#' @export
calc_ss_lin <- function(
  f = "TwoCompOral",
  dose,
  interval,
  model,
  parameters,
  covariates = NULL,
  map = NULL,
  n_days = 45,
  n_transit_compartments = 0,
  auc = FALSE) {

  ## re-map parameters
  if(!is.null(map)) {
    for(key in names(map)) {
      parameters[[key]] <- parameters[[map[[key]]]]
    }
  }

  ## we can use the 2-cmt oral linear model also for models with transit compartments,
  ## the difference for the steady-state trough level vs the "true" trough level
  ## is generally negligible, at least for the state of the system at trough.
  if(!is.null(n_transit_compartments) && n_transit_compartments > 0) {
    mtt <- n_transit_compartments + 1
    parameters$KA <- parameters$KA / mtt # reasonable approximation: ka = 1 / mean transit time
  }

  if(!is.null(covariates)) {
     parameters <- PKPDsim::calculate_parameters(
       ode = model,
       parameters = parameters,
       covariates = covariates
    )
  }

  ## create dataset as input for TwoCompOral ADVAN function
  d <- data.frame(ID = 1,
                  TIME = seq(0, interval * n_days * (24/interval), interval),
                  AMT = dose, EVID = 1, DV = 0,
                  CL = parameters$CL,
                  V2 = parameters$V,
                  Q  = parameters$Q,
                  V3 = parameters$V2,
                  KA = parameters$KA,
                  A1 = 0, A2 = 0, A3 = 0, F1 = 1)
  obs <- tail(d, 1)
  obs$AMT <- 0
  obs$EVID <- 0
  d <- rbind(d, obs)

  ## simulate to steady state using ADVAN function
  res <- do.call(f, list(d = d))
  res$A1 <- res$A1 - dose

  ## Return state vector at steady state trough
  A <- as.numeric(res[res$EVID == 0, c("A1", "A2", "A3")])

  if(!is.null(n_transit_compartments) && n_transit_compartments > 0) {
    A <- c(A[1], rep(0, n_transit_compartments), A[2:3])
  }
  if(auc) {
    A <- c(A, 0)
  }
  return(A)
}
