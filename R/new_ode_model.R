#' Create new ODE model
#'
#' @param model model name from model library
#' @param code C++ code for model
#' @param file file containing C++ code
#' @param func R function to be used with deSolve library
#' @param state_init vector of state init
#' @param parameters list or vector of parameter values
#' @param size size of state vector for model. Size will be extracted automatically from supplied code, use this argument to override.
#' @param obs list with "scale": character string with definition for scale, e.g. "V" or "V*(WT/70)". If NULL, scale defaults to 1., and "cmt" the observation compartment
#' @param dose specify default dose compartment, e.g. list(cmt = 1)
#' @param lagtime either a single number or vector of lagtimes for each compartment. If specified as single number, will apply to default dose compartment. Lagtime can be specified both as number or as parameters, e.g. "ALAG".
#' @param covariates specify covariates, either as a character vector or a list. if specified as list, it allows use of timevarying covariates (see `new_covariate()` function for more info)
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
                           lagtime = NULL,
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
      state_init <- gsub("(#).*?\\n", "\n", state_init) # remove comments
      state_init <- gsub("\\n", ";\n", state_init) # make sure each line has a line ending
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
      parameters <- get_parameters_from_code(code, state_init, declare_variables)
    } else {
      if(class(parameters) == "list") {
        parameters <- names(parameters)
      }
    }
    cov_names <- NULL
    if(!is.null(covariates)) {
      if(class(covariates) == "character") {
        cov_names <- covariates
      }
      if(class(covariates) == "list") {
        cov_names <- names(covariates)
      }
    }
    variables <- declare_variables
    if(!is.null(cov_names)) {
      declare_variables <- c(declare_variables,
                             cov_names,
                             paste0(cov_names, "_0"),
                             paste0("gr_", cov_names),
                             paste0("t_prv_", cov_names))
    }
    code_orig <- code
    code <- gsub("\\r\\n", "\n", code)
    code <- gsub("\\n", ";\n", code)
    code <- gsub("$", ";\n", code)
    code <- gsub("^;", "", code)
    cmp <- compile_sim_cpp(code, size, parameters, cpp_show_code = cpp_show_code, code_init = code_init_text, state_init = state_init, declare_variables = declare_variables, covariates = covariates, obs = obs, dose = dose, verbose = verbose)
    if(exists("sim_wrapper_cpp", envir = globalenv())) {
      sim_out <- sim_wrapper_cpp
    } else {
      message("Compilation failed. Please use verbose=TRUE and cpp_show_code=TRUE arguments to debug.")
    }
    ## handle lag time
    lagtimes <- NULL
    if(!is.null(lagtime)) {
      lagtimes <- rep(0, size)
      dose$cmt
      if(length(lagtime) == 1 && !is.null(dose$cmt)) {
        lagtimes[dose$cmt] <- lagtime
      }
      if(length(lagtime) == size) {
        lagtimes <- lagtime
      }
      parameters <- c(parameters, lagtime[class(lagtime) == "character"]) # if lagtime specified as parameter
    }
    ## add attributes
    reqd <- parameters
    if(!is.null(declare_variables)) {
      reqd <- reqd[!reqd %in% declare_variables]
    }
    if(!is.null(cov_names)) {
      reqd <- reqd[!reqd %in% cov_names]
    }
    attr(sim_out, "code") <- code
    attr(sim_out, "parameters") <- reqd
    attr(sim_out, "covariates") <- cov_names
    attr(sim_out, "variables") <- variables
    attr(sim_out, "cpp")  <- TRUE
    attr(sim_out, "size")  <- size
  }
  attr(sim_out, "obs")  <- obs
  attr(sim_out, "dose") <- dose
  attr(sim_out, "lagtime") <- lagtimes
  class(sim_out) <- c("PKPDsim", class(sim_out))
  return(sim_out)
}
