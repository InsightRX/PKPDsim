#' Create new ODE model
#'
#' @param model model name from model library
#' @param code C++ code for model
#' @param file file containing C++ code
#' @param func R function to be used with deSolve library
#' @param state_init vector of state init
#' @param parameters list of parameter values
#' @param size size of state vector for model. Size will be extracted automatically from supplied code, use this argument to override.
#' @param obs list with "scale": character string with definition for scale, e.g. "V" or "V*(WT/70)". If NULL, scale defaults to 1., and "cmt" the observation compartment
#' @param dose specify default dose compartment, e.g. list(cmt = 1)
#' @param covariates specify covariate values
#' @param declare_variables declare variables
#' @param cpp_show_code show generated C++ code
#' @param verbose show more output
#' @export
new_ode_model <- function (model = NULL,
                           code = NULL,
                           file = NULL,
                           func = NULL,
                           state_init = NULL,
                           parameters = NULL,
                           size = NULL,
                           obs = list("cmt" = 1, scale = 1),
                           dose = list("cmt" = 1),
                           covariates = NULL,
                           declare_variables = NULL,
                           cpp_show_code = FALSE,
                           verbose = FALSE) {
  if (is.null(model) & is.null(code) & is.null(file) & is.null(func)) {
    stop(paste0("Either a model name (from the PKPDsim library), ODE code, an R function, or a file containing code for the ODE system have to be supplied to this function. The following models are availabler in the default library:\n  ", model_library()))
  }
  if (!is.null(func)) { # R function supplied, use deSolve approach
    if(class(func) != "function") {
      stop("The object specified in 'func' is not a valid R function.")
    }
    sim_out <- func
    attr(sim_out, "cpp") <- FALSE
  } else {
    if (!is.null(model)) {
      mod_def <- model_library(model)
      code <- mod_def$code
      obs <- mod_def$obs
      dose <- mod_def$dose
      if(!is.null(mod_def$size)) {
        size <- mod_def$size
      } else {
        size <- get_ode_model_size(mod_def$code)
      }
    }
    code <- gsub("dadt", "dAdt", code)
    code <- gsub("DADT", "dAdt", code)
    code_init_text <- ""
    if(class(state_init) == "character") {
      code_init_text <- paste0(state_init, ";\n")
    }
    if(class(code) == "list") {
      code_tmp <- code
      code <- "" # write new code, combining all list elements
      n <- 0
      for(i in seq(names(code_tmp))) {
        idx <- names(code_tmp)[i]
        tmp <- code_tmp[[idx]]
        if (i > 1) {
          code_tmp[[idx]] <- shift_state_indices(code_tmp[[idx]], n)
          if(class(state_init) == "list" && !is.null(state_init[[idx]])) {
            code_init_text <- paste(code_init_text, shift_state_indices(unlist(state_init[[idx]]), n), ";\n");
          }
        }
        code <- paste(code, code_tmp[[idx]], sep = "\n")
        n <- n + get_ode_model_size(tmp)
      }
    }
    if(is.null(size) && !is.null(code)) {
      size <- get_ode_model_size(code)
      if(size == 0) {
        stop("Sorry, no ODE system detected. Please specify ODEs using 'dAdt[...] = ...'")
      }
    }
    code_init_text <- shift_state_indices(code_init_text, -1)
    if(exists("sim_wrapper_cpp", envir = globalenv())) {
      rm("sim_wrapper_cpp", envir = globalenv())
    }
    if(is.null(size)) {
      size <- get_ode_model_size(code)
    }
    if(is.null(parameters)) {
      parameters <- get_parameters_from_code(code)
    }
    declare_variables <- c(declare_variables, names(covariates))
    code_orig <- code
    code <- gsub("\\r\\n", "\n", code)
    code <- gsub("\\n", ";\n", code)
    code <- gsub("$", ";\n", code)
    code <- gsub("^;", "", code)
    cmp <- compile_sim_cpp(code, size, parameters, cpp_show_code = cpp_show_code, code_init = code_init_text, declare_variables = declare_variables, covariates = covariates, obs = obs, verbose = verbose)
    if(exists("sim_wrapper_cpp", envir = globalenv())) {
      sim_out <- sim_wrapper_cpp
    } else {
      message("Compilation failed. Please use verbose=TRUE and cpp_show_code=TRUE arguments to debug.")
    }
    attr(sim_out, "code") <- code_orig
    attr(sim_out, "parameters") <- cmp$parameters
    attr(sim_out, "cpp")  <- TRUE
    attr(sim_out, "size")  <- size
  }
  attr(sim_out, "obs")  <- obs
  attr(sim_out, "dose") <- dose
  class(sim_out) <- c("PKPDsim", class(sim_out))
  return(sim_out)
}
