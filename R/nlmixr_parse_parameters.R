#' Function to parse parameters for a model into a structure used by nlmixr
#'
#' @param parameters list of parameters
#' @param omega vector describing the lower-diagonal of the between-subject variability matrix
#' @param res_var residual variability. Expected a list with arguments `prop`, `add`, and/or `exp`. NULL by default.
#' @param fixed vector of fixed parameters
#' @param log_transform log-transform estimated parameters in nlmixr?
#' @param ... passed on
#' @export
#' @return List of parameters that can be used by nlmixr
nlmixr_parse_parameters <- function(
  parameters = list(CL = 5, V = 50),
  omega = c(0.1, 0.05, 0.1),
  res_var = list(prop = 0.1, add = 1),
  fixed = c(),
  log_transform = TRUE,
  ...
) {
  par_code <- ""
  model_par_code <- ""
  etas <- c()
  for(key in names(parameters)) {
    if(! key %in% fixed) {
      if(log_transform) {
        par_code <- paste0(par_code, "    log", key, " <- log(", parameters[[key]], ") # log ", key, " \n")
      } else {
        par_code <- paste0(par_code, "    ", key, " <- ", parameters[[key]], " # ", key, " \n")
      }
      etas <- c(etas, paste0("eta_", key))
      if(log_transform) {
        model_par_code <- paste0(model_par_code, paste0("    ", key, " <- exp(log", key, " + eta_", key, ")\n" ))
      } else {
        model_par_code <- paste0(model_par_code, paste0("    ", key, " <- tv_", key, " * exp(eta_", key, ")\n" ))
      }
    } else {
      model_par_code <- paste0(model_par_code, paste0("    ", key, " <- ", parameters[[key]], "\n" ))
    }
  }
  om_text <- paste0("    ",
                    paste(etas, collapse = " + "), " ~ c(\n",
                    "      ",
                    paste(omega, collapse = ", "), ")")
  sig_text <- ""
  if(!is.null(res_var$prop)) sig_text <- paste0(sig_text, "    err_prop <- c(0, ", res_var$prop, ", ", 10 * res_var$prop, ")\n")
  if(!is.null(res_var$add))  sig_text <- paste0(sig_text, "    err_add <- c(0, ", res_var$add, ", ", 10 * res_var$add, ")\n")
  return(
    list(ini = paste0("  ini({\n", par_code, sig_text, om_text, "\n  })"),
         model_par_code = model_par_code)
  )
}
