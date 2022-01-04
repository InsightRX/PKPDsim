#' Combines covariates and parameters into a single list, useful for reparametrization of the model.
#'
#' @param covs covariates object
#' @param pars model parameters, such as the output of the `parameters()` call frmo a model library.
#'
#' @export
#' @return List containing covariates and parameters
join_cov_and_par <- function(covs, pars){
  c(pars, lapply(covs, `[[`, "value"))
}
