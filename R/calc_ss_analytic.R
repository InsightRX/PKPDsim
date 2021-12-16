#' Returns the state of a linear PK system at steady state (trough)
#' using analytics equations (so for linear PK systems only).
#'
#' Basically it performs a PK simulation using analytic equations instead
#' of ODEs to steady state (n=45 days, increased if needed).
#'
#' It can also be used for models with transit compartments, however,
#' the assumption is made that at the end of the dosing interval the
#' amount in the transit compartments is negligible (0).
#'
#' @param f analytic equation to use, must be one of `names(advan_funcs)`
#' @param dose dose
#' @param interval interval
#' @param t_inf infusion time
#' @param model PKPDsim model
#' @param parameters parameters list
#' @param covariates covariates list
#' @param map list for remapping parameters, ex: `list(CL = "CL", V = "V")`
#' @param n_days number of days at which to assume steady state. Default is 45.
#' @param n_transit_compartments number of transit compartments, will insert n compartments between the first (dose) compartment and the second (central) compartment.
#' @param auc add (empty) AUC compartment at end of state vector?
#'
#' @export
#' @return State vector of a linear pharmacokinetic system at steady state
calc_ss_analytic <- function(
  f = "1cmt_oral",
  dose,
  interval,
  t_inf = NULL,
  model,
  parameters,
  covariates = NULL,
  map = NULL,
  n_days = 45,
  n_transit_compartments = 0,
  auc = FALSE) {

  if(!is.null(covariates)) {
    parameters <- PKPDsim::calculate_parameters(
      ode = model,
      parameters = parameters,
      covariates = covariates
    )
  }

  ## re-map parameters
  if(!is.null(map)) {
    for(key in names(map)) {
      parameters[[key]] <- parameters[[map[[key]]]]
    }
  }

  ## we can also use this approach for models with transit compartments,
  ## the difference for the steady-state trough level vs the "true" trough level
  ## is generally negligible, at least for the state of the system at trough.
  if(!is.null(n_transit_compartments) && n_transit_compartments > 0) {
    mtt <- n_transit_compartments + 1
    parameters$KA <- parameters$KA / mtt # reasonable approximation: ka = 1 / mean transit time
  }

  ## create dataset as input for TwoCompOral ADVAN function
  t_dos <- seq(0, interval * n_days * (24/interval), interval)
  d <- data.frame(ID = 1,
                  TIME = t_dos,
                  AMT = dose, EVID = 1, DV = 0,
                  CL = parameters$CL,
                  V = parameters$V,
                  Q  = ifelse0(parameters$Q, NA),
                  V2 = ifelse0(parameters$V2, NA),
                  Q2  = ifelse0(parameters$Q2, NA),
                  V3 = ifelse0(parameters$V3, NA),
                  KA = ifelse0(parameters$KA, NA),
                  A1 = 0, A2 = 0, A3 = 0, A4 = 0, A5 = 0,
                  F1 = ifelse0(parameters$F1, 1))
  if(!is.null(t_inf)) {
    d$RATE <- d$AMT / t_inf
    d <- advan_process_infusion_doses(d)
  }
  obs <- utils::tail(d, 1)
  obs$TIME <- utils::tail(t_dos,1) + interval
  obs$AMT <- 0
  obs$EVID <- 0
  if(!is.null(t_inf)) {
    obs$RATE <- 0
    obs$RATEALL <- 0
  }
  d <- rbind(d, obs)
  d$AUC <- 0

  ## simulate to steady state using ADVAN function
  func <- advan(f, cpp = TRUE)
  res <- func(d)

  ## Return state vector at steady state trough
  A <- as.numeric(res[res$EVID == 0, paste0("A", 1:attr(func, "cmt"))])

  if(attr(func, "type") == "oral") {
    if(!is.null(n_transit_compartments) && n_transit_compartments > 0) {
      A <- c(A[1], rep(0, n_transit_compartments), A[2:length(A)])
    }
  }
  if(auc) { ## add AUC compartment (empty)
    A <- c(A, 0)
  }
  return(A)
}
