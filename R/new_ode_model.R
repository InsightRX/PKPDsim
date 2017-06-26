#' Create new ODE model
#'
#' @param model model name from model library
#' @param code C++ code specifying ODE system
#' @param pk_code C++ code called at any event
#' @param dose_code C++ code called at dose event only
#' @param file file containing C++ code
#' @param func R function to be used with deSolve library
#' @param state_init vector of state init
#' @param parameters list or vector of parameter values
#' @param units list or vector of parameter units
#' @param size size of state vector for model. Size will be extracted automatically from supplied code, use this argument to override.
#' @param lagtime lag time
#' @param obs list with "scale": character string with definition for scale, e.g. "V" or "V*(WT/70)". If NULL, scale defaults to 1., and "cmt" the observation compartment
#' @param dose specify default dose compartment, e.g. list(cmt = 1)
#' @param covariates specify covariates, either as a character vector or a list. if specified as list, it allows use of timevarying covariates (see `new_covariate()` function for more info)
#' @param declare_variables declare variables
#' @param iiv inter-individual variability, can optionally be added to library
#' @param iov inter-occasion variability, can optionally be added to library
#' @param omega_matrix variance-covariance matrix for inter-individual variability, can optionally be added to library
#' @param ruv residual variability, can optionally be added to library
#' @param default_parameters population or specific patient values, can optionally be added to library
#' @param cpp_show_code show generated C++ code
#' @param package package name when saving as package
#' @param install install package after compilation?
#' @param folder base folder name to create package in
#' @param lib_location install into folder (`--library` argument)
#' @param verbose show more output
#' @param as_is use C-code as-is, don't substitute line-endings or shift indices
#' @export
new_ode_model <- function (model = NULL,
                           code = NULL,
                           pk_code = NULL,
                           dose_code = NULL,
                           file = NULL,
                           func = NULL,
                           state_init = NULL,
                           parameters = NULL,
                           units = NULL,
                           size = NULL,
                           lagtime = NULL,
                           obs = list("cmt" = 1, "scale" = 1),
                           dose = list("cmt" = 1),
                           covariates = NULL,
                           declare_variables = NULL,
                           iiv = NULL,
                           iov = NULL,
                           omega_matrix = NULL,
                           ruv = NULL,
                           default_parameters = NULL,
                           cpp_show_code = FALSE,
                           package = NULL,
                           install = TRUE,
                           folder = NULL,
                           lib_location = NULL,
                           verbose = FALSE,
                           as_is = FALSE
                          ) {
  if (is.null(model) & is.null(code) & is.null(file) & is.null(func)) {
    stop(paste0("Either a model name (from the PKPDsim library), ODE code, an R function, or a file containing code for the ODE system have to be supplied to this function. The following models are available:\n  ", model_library()))
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
    code_init_text <- ""
    if(class(state_init) == "character") {
      code <- gsub("dadt", "dAdt", code)
      code <- gsub("DADT", "dAdt", code)
      code <- gsub("^\\n", "", code)
      code <- gsub("^(\\s)*", "", code)
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
        tmp <- gsub("dadt", "dAdt", tmp)
        tmp <- gsub("DADT", "dAdt", tmp)
        # tmp <- gsub("^\\n", "", tmp)
        # tmp <- gsub("^(\\s)*", "", tmp)
        if (i > 1) {
          code_tmp[[idx]] <- shift_state_indices(code_tmp[[idx]], n)
          if(class(state_init) == "list" && !is.null(state_init[[idx]])) {
            state_init[[idx]] <- gsub("(#).*?\\n", "\n", state_init[[idx]]) # remove comments
            state_init[[idx]] <- gsub("\\n", ";\n", state_init[[idx]]) # make sure each line has a line ending
            code_init_text <- paste(code_init_text, shift_state_indices(unlist(state_init[[idx]]), n), ";\n");
          }
        }
        code <- paste(code, paste0(code_tmp[[idx]]), sep = "\n")
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
    if(is.null(size)) {
      size <- get_ode_model_size(code)
    }
    if(is.null(parameters)) {
      comb_code <- code
      if(!is.null(pk_code)) {
        comb_code <- paste(code, pk_code, sep = "\n")
      }
      parameters <- get_parameters_from_code(comb_code, state_init, declare_variables)
    } else {
      if(class(parameters) == "list") {
        parameters <- names(parameters)
      }
    }
    if(is.null(obs$scale)) { obs$scale <- 1 }
    if(is.null(obs$cmt))   { obs$cmt <- 1 }
    if(is.null(dose$cmt))  { dose$cmt <- 1 }
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
    code <- cleanup_code(code)
    pk_code <- cleanup_code(pk_code)
    dose_code <- cleanup_code(dose_code)
    compile <- TRUE
    if(!is.null(package)) { # don't compile if saving as library
      compile = FALSE
    }
    cmp <- compile_sim_cpp(code = code,
                           pk_code = pk_code,
                           dose_code = dose_code,
                           size = size,
                           p = parameters,
                           cpp_show_code = cpp_show_code,
                           code_init = code_init_text,
                           state_init = state_init,
                           declare_variables = declare_variables,
                           variables = variables,
                           covariates = covariates,
                           obs = obs,
                           dose = dose,
                           verbose = verbose,
                           compile = compile,
                           as_is = as_is)
    reqd <- parameters
    if(length(grep("cov_", reqd)) > 0) {
      reqd <- reqd[-grep("cov_", reqd)]
    }
    if(!is.null(declare_variables)) {
      reqd <- reqd[!reqd %in% declare_variables]
    }
    if(!is.null(cov_names)) {
      reqd <- reqd[!reqd %in% cov_names]
    }
    if(exists("sim_wrapper_cpp", envir = globalenv())) {
      if(compile) {
        sim_out <- get("sim_wrapper_cpp")
      }
      rm("sim_wrapper_cpp", envir = globalenv())
    }
    if(compile) {
      attr(sim_out, "code") <- code
      if(!is.null(pk_code)) {
        attr(sim_out, "pk_code") <- pk_code
      }
      attr(sim_out, "parameters") <- reqd
      attr(sim_out, "covariates") <- cov_names
      attr(sim_out, "variables") <- variables
      attr(sim_out, "cpp")  <- TRUE
      attr(sim_out, "size")  <- size
      attr(sim_out, "obs")  <- obs
      attr(sim_out, "dose") <- dose
      attr(sim_out, "lagtime") <- lagtime
      class(sim_out) <- c("PKPDsim", class(sim_out))
      return(sim_out)
    }

    ## save to library, if requested:
    if(!is.null(package)) {
      if(is.null(folder)) {
        folder <- tempdir()
      }

      ## Copy template files
      new_folder <- paste0(folder, "/", package)
      templ_folder <- paste0(system.file(package="PKPDsim"), "/template")
      templ_folder <- paste0(templ_folder, "/", dir(templ_folder))
      if(!file.exists(new_folder)) {
        dir.create(new_folder)
      }
      file.copy(from = templ_folder, to = new_folder,
                overwrite = TRUE, recursive = TRUE, copy.mode = FALSE)

      ## Write new source file
      fileConn <- file(paste0(new_folder, "/src/sim_wrapper_cpp.cpp"))
      writeLines(cmp$cpp, fileConn)
      close(fileConn)

      ## Replace module name and other info
      if(is.null(pk_code)) { pk_code <- "" }
      if(is.null(dose_code)) { dose_code <- "" }
      if(is.null(lagtime)) { lagtime <- "NULL" }
      if(is.null(obs$cmt)) { obs$cmt <- "1" }
      if(is.null(obs$scale)) { obs$scale <- "1" }
      if(is.null(dose$cmt)) { dose$cmt <- "1" }
      if(is.null(dose$bioav)) { dose$bioav <- "1" }
      if(is.null(size)) { size <- "1" }
      pars <- paste0("c(", paste(add_quotes(reqd), collapse = ", "), ")")
      covs <- paste0("c(", paste(add_quotes(cov_names), collapse = ", "), ")")
      vars <- paste0("c(", paste(add_quotes(variables), collapse = ", "), ")")
      repl <- matrix(c("\\[MODULE\\]", package,
                       "\\[N_COMP\\]", size,
                       "\\[OBS_COMP\\]", obs$cmt,
                       "\\[DOSE_COMP\\]", dose$cmt,
                       "\\[OBS_SCALE\\]", obs$scale,
                       "\\[DOSE_BIOAV\\]", dose$bioav,
                       "\\[CODE\\]", code,
                       "\\[PK_CODE\\]", pk_code,
                       "\\[DOSE_CODE\\]", dose_code,
                       "\\[PARS\\]", pars,
                       "\\[VARS\\]", vars,
                       "\\[COVS\\]", covs,
                       "\\[LAGTIME\\]", lagtime
      ), ncol=2, byrow=TRUE)
      if(verbose) {
        print(repl)
      }
      search_replace_in_file(paste0(new_folder, "/R/model.R"), repl[,1], repl[,2])
      if(is.null(iiv)) { iiv <- "" } else {
        iiv <- PKPDsim::print_list(iiv, FALSE)
      }
      if(is.null(iov)) { iov <- "" } else {
        iov <- PKPDsim::print_list(iov, FALSE)
      }
      if(is.null(ruv)) { ruv <- "" } else {
        ruv <- PKPDsim::print_list(ruv, FALSE)
      }
      if(is.null(omega_matrix)) {
        omega_matrix <- ""
      } else {
        omega_matrix <- paste0("c(", paste(omega_matrix, collapse = ", "), ")")
      }
      if(is.null(default_parameters)) { default_parameters <- "" } else {
        default_parameters <- PKPDsim::print_list(default_parameters, FALSE)
      }
      if(is.null(units)) { units <- "''" } else {
        units <- PKPDsim::print_list(units, TRUE, TRUE)
      }
      search_replace_in_file(paste0(new_folder, "/R/iiv.R"), "\\[IIV\\]", iiv)
      search_replace_in_file(paste0(new_folder, "/R/iov.R"), "\\[IOV\\]", iov)
      search_replace_in_file(paste0(new_folder, "/R/omega_matrix.R"), "\\[OMEGA_MATRIX\\]", omega_matrix)
      search_replace_in_file(paste0(new_folder, "/R/parameters.R"), c("\\[PARAMETERS\\]", "\\[UNITS\\]"), c(default_parameters, units))
      search_replace_in_file(paste0(new_folder, "/R/ruv.R"), "\\[RUV\\]", ruv)
      search_replace_in_file(paste0(new_folder, "/DESCRIPTION"), "\\[MODULE\\]", package)
      search_replace_in_file(paste0(new_folder, "/NAMESPACE"), "\\[MODULE\\]", package)
      search_replace_in_file(paste0(new_folder, "/man/modulename-package.Rd"), "\\[MODULE\\]", package)
      file.rename(paste0(new_folder, "/man/modulename-package.Rd"), paste0(new_folder, "/man/", package, ".Rd"))

      ## Compile / build / install
      curr <- getwd()
      setwd(new_folder)
      if(file.exists(paste0(new_folder, "/R/RcppExports.R"))) {
        file.remove(paste0(new_folder, "/R/RcppExports.R"))
      }
      if(file.exists(paste0(new_folder, "/src/RcppExports.cpp"))) {
        file.remove(paste0(new_folder, "/src/RcppExports.cpp"))
      }
      Rcpp::compileAttributes(".", )
      if(install) { # install into R
        lib_location_arg <- ""
        if(!is.null(lib_location)) {
          lib_location_arg <- paste0("--library=", lib_location)
        }
        system(paste0("R CMD INSTALL ", lib_location_arg, " --no-multiarch --with-keep.source ."))
      } else { # build to zip file
        system(paste0("R CMD build ."))
        pkg_file <- paste0(new_folder, "/", package, "_1.0.tar.gz")
        pkg_newfile <- paste0(curr, "/", package, "_PKPDsim.tar.gz")
        if(file.exists(pkg_file)) {
          file.copy(pkg_file, pkg_newfile)
          message(paste0("Package built in: ", pkg_newfile))
        } else {
          message("Package not created.")
        }
      }
      setwd(curr)
    }
  }

}
