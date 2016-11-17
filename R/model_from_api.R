#' Load model definition from API, and compile to R library
#'
#' @param model model id
#' @param url URL of API
#' @param verbose verbosity (T/F)
#' @param definition_only return only the model definition, do not compile
#' @param force force install, even if model inactive
#' @export
model_from_api <- function(model = NULL,
                           url = "http://localhost:8080/api",
                           verbose = TRUE,
                           get_definition = FALSE,
                           to_package = FALSE,
                           force = FALSE,
                           ...) {
  if(is.null(model)) {
    defs <- jsonlite::fromJSON(paste0(url, "/models"))
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
  if(get_definition) {
    return(def)
  }
  if(def$build || force) {
    if(verbose) {
      message("Compiling model.")
    }
    package <- NULL
    if(to_package) {
      package <- gsub("_", "", def$id)
    }
    mod <- PKPDsim::new_ode_model(code = def$ode_code,
                                  pk_code = def$pk_code,
                                  dose_code = def$dose_code,
                                  obs = def$obs,
                                  dose = def$dose,
                                  size = def$n_comp,
                                  parameters = def$parameters,
                                  declare_variables = def$variables,
                                  covariates = def$covariates,
                                  as_is = TRUE,
                                  package = package,
                                  iiv = def$iiv,
                                  omega_matrix = def$omega_matrix,
                                  ruv = def$ruv,
                                  default_parameters = def$default_parameters,
                                  ...)
    if(is.null(mod)) {
      message("Done.")
    }
    return(mod)
  }
  if(!def$build) {
    message("Model not flagged for building, skipping compilation.")
  }
}
