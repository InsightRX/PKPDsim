#' Reparametrize model parameters using a reparametrization defined within the model.
#'
#' Mostly useful for reparametrizing models into standard parametrizations, e.g. to
#' NONMEM TRANS or clinPK parametrizations.
#'
#' @param model PKPDsim model, compiled using `reparametrization` argument or in metadata object.
#' @param parameters list of model parameters
#' @param covariates covariates list, specified as PKPDsim covariates
#'
#' @export
#' @return Reparameterized model parameters
reparametrize <- function(model, parameters, covariates) {
  reparametrization <- attr(model, "reparametrization")
  if(is.null(reparametrization)) {
    warning("No reparametrization found, returning original parameters object.")
    return(parameters)
  }
  if(!is.null(covariates)) {
    parameters <- c(parameters, lapply(covariates, function(x) { tail(x$value) } ))
  }
  with(parameters, {
    repar <- list()
    for(n in names(reparametrization)) {
      repar[n] <- eval(parse(text = paste0(reparametrization[n])))
    }
    return(repar)
  })
}
