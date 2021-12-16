#' Load model definition from API, and compile to R library
#'
#' @param url URL or file path to JSON representation of model
#' @param model model id (used in messages)
#' @param nonmem URL or file path to NONMEM file
#' @param verbose verbosity (T/F)
#' @param get_definition return only the model definition, do not compile
#' @param to_package compile to package?
#' @param force force install even if same version number of model already installed.
#' @param install_all force install all, even if model inactive
#' @param ... arguments passed to `new_ode_model()` function
#' @export
#' @return Model object created with [PKPDsim::new_ode_model()]
#' @md
model_from_api <- function(url,
                           model = NULL,
                           nonmem = NULL,
                           verbose = TRUE,
                           get_definition = FALSE,
                           to_package = FALSE,
                           force = FALSE,
                           install_all = FALSE,
                           ...) {
  def <- read_model_json(url)
  if(verbose) {
    message("- Retrieving model definition (", model, ")...")
  }
  if(is.null(def) || is.null(def$ode_code)) {
    stop("Returned model definition is empty, please make sure the specified URL or path is correct.")
  }
  if(get_definition) {
    return(def)
  }
  if(!def$build && !install_all) {
    message(paste0("- Model ", model, " not flagged for building, skipping compilation. Use `install_all=TRUE` to force build."))
  }
  if(isTRUE(def$misc$init_parameter) && !is.null(def$misc$model_type)) {
    ## Add a parameter and initialization code for setting the initial concentration based on a TDM value
    def <- define_tdm_init_model(def)
  }
  if(!is.null(nonmem) && !is.null(def$implementations$nonmem)) {
    nonmem <- paste(readLines(nonmem), collapse="\n")
  }
  if(is.null(def$comments)) def$comments <- ""
  mod <- NULL
  if(def$build || install_all) {
    build <- TRUE
    package <- NULL
    if(to_package) {
      package <- gsub("_", "", def$id)
    }
    if(!force) {
      build <- is_newer_package(package, def$version)
    }
    if(build) {
      if(verbose) {
        message("- Compiling model.")
      }
      mod <- PKPDsim::new_ode_model(
        code = def$ode_code,
        pk_code = def$pk_code,
        dose_code = def$dose_code,
        obs = def$obs,
        dose = def$dose,
        size = def$n_comp,
        parameters = names(def$parameters),
        reparametrization = def$reparametrization,
        mixture = def$mixture,
        units = def$units,
        declare_variables = def$variables,
        covariates = def$covariates,
        as_is = TRUE,
        package = package,
        iiv = def$iiv,
        iov = def$iov,
        omega_matrix = def$omega_matrix,
        ruv = def$ruv,
        ltbs = def$ltbs,
        misc = def$misc,
        cmt_mapping = def$cmt_mapping,
        lagtime = def$lagtime,
        default_parameters = def$parameters,
        fixed = def$fixed,
        state_init = def$state_init,
        verbose = verbose,
        nonmem = nonmem,
        int_step_size = def$simulation$int_step_size,
        comments = stringr::str_replace_all(def$comments, '\\"', ""),
        version = ifelse(!is.null(def$version), def$version, "0.1.0"),
        ...
      )
    }
  }
  if(!is.null(mod)) {
    return(mod)
  }
}
