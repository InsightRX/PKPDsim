#' Reparametrize model parameters using a reparametrization defined within the model.
#'
#' Mostly useful for reparametrizing models into standard parametrizations, e.g. to
#' NONMEM TRANS or clinPK parametrizations.
#'
#' @param parameters list of model parameters
#' @param model PKPDsim model, compiled using `reparametrization` argument or in metadata object.
#'
#' @export
reparametrize <- function(parameters, model) {
  reparametrization <- attr(model, "reparametrization")
  if(is.null(reparametrization)) {
    warning("No reparametrization found, returning original parameters object.")
    return(parameters)
  }
  with(parameters, {
    repar <- list()
    for(n in names(reparametrization)) {
      repar[n] <- eval(parse(text = paste0(reparametrization[n])))
    }
    return(repar)
  })
}
