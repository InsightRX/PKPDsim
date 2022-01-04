#' Convert a model generated with PKPDsim to an object for nlmixr
#'
#' @param model PKPDsim model
#' @param parameters list of parameters
#' @param omega vector describing the lower-diagonal of the between-subject variability matrix
#' @param res_var residual variability. Expected a list with arguments `prop`, `add`, and/or `exp`. NULL by default.
#' @param fixed vector of fixed (not estimated) parameter names
#' @param ini_code manually specify the `ini` block for nlmixr
#' @param model_code manually specify the `model` block for nlmixr
#' @param model_par_code manually specify the parameters section inside the `model` block for nlmixr
#' @param verbose verbose, `TRUE` or `FALSE`
#' @param ... passed on
#' @export
#' @return nlmixr function
pkpdsim_to_nlmixr <- function(
  model = NULL,
  parameters = NULL,
  omega = NULL,
  res_var = NULL,
  fixed = c(),
  ini_code = NULL,
  model_code = NULL,
  model_par_code = NULL,
  verbose = FALSE,
  ...) {

  ## Checks
  if(is.null(model) || is.null(parameters) || is.null(omega) || is.null(res_var)) {
    stop("The `model`, `parameters`, `omega`, and `res_var` arguments are mandatory!")
  }

  ## Ini block
  if(is.null(ini_code)) {
    if(verbose) message("Converting parameters to nlmixr object.")
    params <- nlmixr_parse_parameters(
      parameters = parameters, omega = omega, rew_var = res_var,
      fixed = fixed, ...)
    ini_code <- params$ini
    model_par_code <- params$model_par_code
  } else {
    ini_code <- paste0("  ini({\n", ini_code, "\n  })\n")
  }

  ## Model block
  if(is.null(model_code)) {
    if("PKPDsim" %in% class(model)) {
      model_code <- PKPDsim::translate_ode(attr(model, "code"), from = "PKPDsim", to = "RxODE", verbose = verbose)
    } else {
      stop("This doesn't seem to be a valid PKPDsim model.")
    }
  }
  dv_def <- paste0("    y = A", attr(model, "obs")$cmt, "/", attr(model, "obs")$scale)
  ruv_code <- paste("    y ~ ",
                    paste(lapply(names(res_var), function(x) return(paste0(x, "(err_", x, ")"))), collapse = " + "))
  model_code <- paste0("  model({\n",
                       model_par_code,
                       model_code, "\n",
                       dv_def, "\n",
                       ruv_code, "\n",
                       "  })\n")
  txt <- paste0(
    "function() { \n",
    ini_code,
    "\n",
    model_code,
    "}\n")
  if(verbose) {
    message("Generating nlmixr function.\n")
    message(txt)
  }
  return(eval(parse(text = txt)))
}
