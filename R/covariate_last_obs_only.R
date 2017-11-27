#' Use only last observed covariate values
#'
#' @param covariates covariates object
#' @export
covariate_last_obs_only <- function(covariates) {
  for(lab in names(covariates)) {
    covariates[[lab]]$value <- tail(covariates[[lab]]$value, 1)
    covariates[[lab]]$times <- 0
    if(!is.null(covariates[[lab]]$value)) {
      covariates[[lab]]$unit <- tail(covariates[[lab]]$unit, 1)
    }
  }
  return(covariates)
}
