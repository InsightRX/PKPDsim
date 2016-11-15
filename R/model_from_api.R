#' Load model definition from API, and compile to R library
#'
#' @param model model id
#' @param url URL of API
#' @export
model_from_api <- function(model = NULL,
                           url = NULL,
                           verbose = TRUE,
                           ...) {
  if(is.null(model)) {
    defs <- fromJSON(paste0(url, "/models"))
    message("No `model` specified, returning available PKPDsim models.")
    return(defs)
  }

  if(verbose) {
    message("Connecting to API...")
  }
  def <- jsonlite::fromJSON(paste0(url, "/model/", model))
  if(verbose) {
    message("Retrieving model definition (", model, ")...")
  }
  if(is.null(def) || is.null(def$ode_code)) {
    stop("Returned model definition is empty, please make sure the specified model id is correct.")
  }
  if(verbose) {
    message("Compiling model.")
  }
  mod <- PKPDsim::new_ode_model(code = def$ode_code,
                         pk_code = def$pk_code,
                         dose_code = def$dose_code,
                         parameters = def$parameters,
                         declare_variables = def$variables,
                         covariates = def$covariates,
                         as_is = TRUE,
                         ...)
  if(is.null(mod)) {
    message("Done.")
  }
  return(mod)
}
