#' Calculate model-specific variables using a dummy call to sim_ode()
#'
#' This is a convenience function for PKPDsim users, it is not used inside the `sim_ode()``
#' function in any way.
#' This function is useful for converting from an estimated parameter to actual parameter,
#' e.g. when clearance is specified as `CLi = CL * (WT/70) * (1/CR)` it can be used to
#' calculate `CLi` without having to write that function a second time in R.
#'
#' @param ode PKPDsim model object
#' @param parameters parameter list
#' @param covariates covariate list. Make sure to include covariates at the right time point, since only last observed covariate values are used.
#' @param include_parameters boolean, include parameters?
#' @param include_variables boolean, include variables?
#' @param ... arguments to pass on to simulation function
#' @export
#' @return List of model-specific variables
calculate_parameters <- function(
  ode = NULL,
  parameters = NULL,
  covariates = NULL,
  include_parameters = TRUE,
  include_variables = TRUE,
  ...
) {
  reg <- new_regimen(amt = 0, times = 0, cmt = 1, type = "bolus")
  incl <- list(parameters = FALSE, variables = FALSE)
  if(!is.null(covariates)) covariates <- covariate_last_obs_only(covariates) # make sure only single value!
  if(include_parameters) incl$parameters <- TRUE
  if(include_variables) incl$variables <- TRUE
  res <- sim_ode(ode = ode,
                 parameters = parameters,
                 regimen = reg,
                 omega = NULL,
                 covariates = covariates,
                 output_include = incl,
                 t_obs = c(0,1), only_obs = TRUE, ...) %>% utils::tail(1)
  return(pars <- as.list(res[,-c(1:4)]))
}
