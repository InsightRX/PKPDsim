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
#' @param reparametrization list of parameters with definitions that reparametrize the linear PK model to a 1-, 2- o4 3-compartment PK with standardized parametrization.
#' @param mixture for mixture models, provide a list of the parameter associated with the mixture and it's possible values and probabilities (of the first value), e.g. `list(CL = list(value = c(10, 20), probability = 0.3)`.
#' @param units list or vector of parameter units
#' @param size size of state vector for model. Size will be extracted automatically from supplied code, use this argument to override.
#' @param lagtime lag time
#' @param obs list with "scale": character string with definition for scale, e.g. "V" or "V*(WT/70)". If NULL, scale defaults to 1., and "cmt" the observation compartment
#' @param dose specify default dose compartment, e.g. list(cmt = 1)
#' @param covariates specify covariates, either as a character vector or a list. if specified as list, it allows use of timevarying covariates (see `new_covariate()` function for more info)
#' @param declare_variables declare variables
#' @param fixed parameters that should not have iiv added.
#' @param iiv inter-individual variability, can optionally be added to library
#' @param iov inter-occasion variability, can optionally be added to library
#' @param omega_matrix variance-covariance matrix for inter-individual variability, can optionally be added to library
#' @param ruv residual variability, can optionally be added to library
#' @param ltbs log-transform both sides. Not used in simulations, only for fitting (sets attribute `ltbs`).
#' @param misc a list of miscellaneous model metadata
#' @param cmt_mapping list indicating which administration routes apply to which compartments. Example: `list("oral" = 1, "infusion" = 2)`
#' @param int_step_size step size for integrator. Can be pre-specified for model, to override default for `sim_ode()`
#' @param default_parameters population or specific patient values, can optionally be added to library
#' @param cpp_show_code show generated C++ code
#' @param package package name when saving as package
#' @param test_file optional test file to be included with package
#' @param install install package after compilation?
#' @param folder base folder name to create package in
#' @param lib_location install into folder (`--library` argument)
#' @param verbose show more output
#' @param as_is use C-code as-is, don't substitute line-endings or shift indices
#' @param nonmem add nonmem code as attribute to model object
#' @param comments comments for model
#' @param version number of library
#' @param quiet passed on to `system2` as setting for stderr and stdout; how to
#' output cmd line output. Default (`""`) is R console, NULL or FALSE discards.
#' TRUE captures the output and saves as a file.
#' @export
#' @return If package name is NULL, returns the model object. Otherwise has no
#'   return value.
new_ode_model <- function (model = NULL,
                           code = NULL,
                           pk_code = NULL,
                           dose_code = NULL,
                           file = NULL,
                           func = NULL,
                           state_init = NULL,
                           parameters = NULL,
                           reparametrization = NULL,
                           mixture = NULL,
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
                           ltbs = NULL,
                           misc = NULL,
                           cmt_mapping = NULL,
                           int_step_size = NULL,
                           default_parameters = NULL,
                           fixed = NULL,
                           cpp_show_code = FALSE,
                           package = NULL,
                           test_file = NULL,
                           install = TRUE,
                           folder = NULL,
                           lib_location = NULL,
                           verbose = FALSE,
                           as_is = FALSE,
                           nonmem = NULL,
                           comments = NULL,
                           version = "0.1.0",
                           quiet = ""
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
    code <- gsub("(#).*?\\n", "\n", code) # remove comments
    if(class(state_init) == "character") {
      code <- gsub("dadt", "dAdt", code)
      code <- gsub("DADT", "dAdt", code)
      code <- gsub("^\\n", "", code)
      code <- gsub("^(\\s)*", "", code)
      state_init <- gsub("(#).*?\\n", "\n", state_init) # remove comments
      state_init <- gsub("\\n", ";\n", state_init) # make sure each line has a line ending
      code_init_text <- paste0(state_init, ";\n")
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

    ## IIV
    if(!is.null(omega_matrix)) {
       if(!is_positive_definite(omega_matrix)) {
           stop("Specified omega matrix is not positive definite.")
       }
    }

    ## IOV
    use_iov <- FALSE
    if(!is.null(iov)) {
      check_iov_specification(iov, code, pk_code)
      # add parameters
      for(i in rev(seq(iov$cv))) {
        txt <- paste0("    kappa_", names(iov$cv)[i], " = 1e-6;\ \n")
        if(length(grep(paste0("kappa_", names(iov$cv)[i]), code)) > 0) {
          for(j in 1:iov$n_bins) {
            txt_occ <- paste0("    if(t >= iov_bin[",(j-1),"] && t < iov_bin[",j,"]) { kappa_", names(iov$cv)[i], " = kappa_", names(iov$cv)[i], "_", j, " + 1e-6; } \ \n")
            txt <- paste0(txt, txt_occ)
            par_tmp <- paste0("kappa_", names(iov$cv)[i], "_", j)
            if(! par_tmp %in% parameters) {
              parameters <- c(parameters, par_tmp)
            }
            default_parameters[[par_tmp]] <- 0
          }
          code <- paste0(txt, code)
        }
        txt <- paste0("    kappa_", names(iov$cv)[i], " = 1e-6;\ \n")
        if(length(grep(paste0("kappa_", names(iov$cv)[i]), pk_code)) > 0) {
          for(j in 1:iov$n_bins) {
            txt_occ <- paste0("    if(times[i] >= iov_bin[",(j-1),"] && times[i] < iov_bin[",j,"]) { kappa_", names(iov$cv)[i], " = kappa_", names(iov$cv)[i], "_", j, "; } \ \n")
            txt <- paste0(txt, txt_occ)
            par_tmp <- paste0("kappa_", names(iov$cv)[i], "_", j)
            if(! par_tmp %in% parameters) {
              parameters <- c(parameters, par_tmp)
            }
            default_parameters[[par_tmp]] <- 0
          }
          pk_code <- paste0(txt, pk_code)
        }
        var_tmp <- paste0("kappa_", names(iov$cv)[i])
        if(! var_tmp %in% declare_variables) {
          declare_variables <- c(declare_variables, var_tmp)
        }
      }
    } else {
      iov <- list(n_bins = 1) # dummy
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

    check_mixture_model(mixture, parameters)

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
                           as_is = as_is,
                           iov = iov)
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
      attr(sim_out, "mixture") <- mixture
      attr(sim_out, "reparametrization") <- reparametrization
      attr(sim_out, "covariates") <- cov_names
      attr(sim_out, "variables") <- variables
      attr(sim_out, "fixed") <- fixed
      attr(sim_out, "cpp")  <- TRUE
      attr(sim_out, "size")  <- size
      attr(sim_out, "obs")  <- obs
      attr(sim_out, "dose") <- dose
      attr(sim_out, "lagtime") <- lagtime
      attr(sim_out, "ltbs") <- ltbs
      attr(sim_out, "misc") <- misc
      attr(sim_out, "cmt_mapping") <- cmt_mapping
      attr(sim_out, "iov") <- iov
      attr(sim_out, "comments") <- paste0("\n", as.character(paste0(paste0(" - ", comments), collapse = "\n")))
      if(!is.null(int_step_size)) {
        attr(sim_out, "int_step_size") <- int_step_size
      }
      class(sim_out) <- c("PKPDsim", class(sim_out))
      return(sim_out)
    }

    ## save to library, if requested:
    if(!is.null(package)) {
      if(is.null(folder)) {
        folder <- tempdir()
      }

      ## Copy template files
      new_folder <- file.path(folder, package)
      templ_folder <- file.path(system.file(package="PKPDsim"), "template")
      templ_folder <- file.path(templ_folder, dir(templ_folder))
      if(!file.exists(new_folder)) {
        dir.create(new_folder)
      }
      file.copy(from = templ_folder, to = new_folder,
                overwrite = TRUE, recursive = TRUE, copy.mode = FALSE)

      ## Write new source file
      fileConn <- file(file.path(new_folder, "src", "sim_wrapper_cpp.cpp"))
      writeLines(cmp$cpp, fileConn)
      close(fileConn)

      ## Replace module name and other info
      if(is.null(pk_code)) { pk_code <- "" }
      if(is.null(dose_code)) { dose_code <- "" }
      if(is.null(lagtime)) { lagtime <- "NULL" } else {
        lagtime <- paste0("c(", paste(add_quotes(lagtime), collapse = ", "), ")")
      }
      if(is.null(obs$cmt)) { obs$cmt <- "1" }
      if(is.null(obs$scale)) { obs$scale <- "1" }
      if(is.null(dose$cmt)) { dose$cmt <- "1" }
      if(is.null(dose$bioav)) { dose$bioav <- "1" }
      if(class(dose$bioav) == "character") {
        dose$bioav <- paste0('\\"', dose$bioav, '\\"')
      }
      if(is.null(size)) { size <- "1" }
      if(is.null(ltbs)) { ltbs <- FALSE }
      if(is.null(state_init)) { state_init <- "NULL" } else { state_init <- add_quotes(state_init)}
      if(is.null(nonmem)) { nonmem <- "NULL" }
      if(is.null(int_step_size)) { int_step_size <- "NULL" }
      pars <- paste0("c(", paste(add_quotes(reqd), collapse = ", "), ")")
      covs <- paste0("c(", paste(add_quotes(cov_names), collapse = ", "), ")")
      fixed <- ifelse(
        is.null(fixed),
        "NULL",
        paste0("c(", paste(add_quotes(fixed), collapse = ", "), ")")
      )
      vars <- paste0("c(", paste(add_quotes(variables), collapse = ", "), ")")
      repl <- matrix(c("\\[MODULE\\]", package,
                       "\\[N_COMP\\]", size,
                       "\\[OBS_COMP\\]", obs$cmt,
                       "\\[DOSE_COMP\\]", dose$cmt,
                       "\\[OBS_SCALE\\]", obs$scale,
                       "\\[OBS_VARIABLE\\]", deparse(obs$variable),
                       "\\[DOSE_BIOAV\\]", dose$bioav,
                       "\\[CODE\\]", code,
                       "\\[PK_CODE\\]", pk_code,
                       "\\[DOSE_CODE\\]", dose_code,
                       "\\[STATE_INIT\\]", state_init,
                       "\\[PARS\\]", pars,
                       "\\[REPARAM\\]", paste0(deparse(reparametrization), collapse = ""),
                       "\\[MIXTURE\\]", paste0(deparse(mixture), collapse = ""),
                       "\\[VARS\\]", vars,
                       "\\[COVS\\]", covs,
                       "\\[FIXED\\]", fixed,
                       "\\[LAGTIME\\]", lagtime,
                       "\\[USE_IOV\\]", as.character(use_iov),
                       "\\[IOV\\]", PKPDsim::print_list(iov, FALSE),
                       "\\[LTBS\\]", as.character(ltbs),
                       "\\[MISC\\]", paste0(deparse(misc), collapse = ""),
                       "\\[CMT_MAPPING\\]", paste0(deparse(cmt_mapping), collapse = ""),
                       "\\[INT_STEP_SIZE\\]", as.character(int_step_size),
                       "\\[COMMENTS\\]", paste0("\n", as.character(paste0(paste0(" - ", comments), collapse = "\n"))),
                       "\\[NONMEM\\]", as.character(nonmem)
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
      search_replace_in_file(file.path(new_folder, "R", "iiv.R"), "\\[IIV\\]", iiv)
      search_replace_in_file(file.path(new_folder, "R", "iov.R"), "\\[IOV\\]", iov)
      search_replace_in_file(file.path(new_folder, "R", "omega_matrix.R"), "\\[OMEGA_MATRIX\\]", omega_matrix)
      search_replace_in_file(file.path(new_folder, "R", "parameters.R"), c("\\[PARAMETERS\\]", "\\[UNITS\\]"), c(default_parameters, units))
      search_replace_in_file(file.path(new_folder, "R", "fixed.R"), "\\[FIXED\\]", fixed)
      search_replace_in_file(file.path(new_folder, "R", "ruv.R"), "\\[RUV\\]", ruv)
      search_replace_in_file(file.path(new_folder, "DESCRIPTION"), "\\[MODULE\\]", package)
      search_replace_in_file(file.path(new_folder, "DESCRIPTION"), "\\[VERSION\\]", version)
      search_replace_in_file(file.path(new_folder, "NAMESPACE"), "\\[MODULE\\]", package)
      search_replace_in_file(file.path(new_folder, "man", "modulename-package.Rd"), "\\[MODULE\\]", package)
      file.rename(
        file.path(new_folder, "man", "modulename-package.Rd"),
        file.path(new_folder, "man", paste0(package, ".Rd"))
      )

      ## copy test file into package
      if(!is.null(test_file)) {
        if(file.exists(test_file[1])) {
          t_dir <- file.path(new_folder, "tests")
          if(!file.exists(t_dir)) dir.create(t_dir)
          file.copy(test_file[1], file.path(t_dir, paste0(package, ".R")))
        } else {
          warning("Specified test file not found.")
        }
      }

      ## Compile / build / install
      if(file.exists(file.path(new_folder, "R", "RcppExports.R"))) {
        file.remove(paste0(new_folder, "R", "RcppExports.R"))
      }
      if(file.exists(file.path(new_folder, "src", "RcppExports.cpp"))) {
        file.remove(file.path(new_folder, "src", "RcppExports.cpp"))
      }
      Rcpp::compileAttributes(new_folder)

      cmd <- file.path(Sys.getenv("R_HOME"), "bin", "R")
      if (install) { # install into R
        lib_location_arg <- ""

        if(!is.null(lib_location)) {
          lib_location_arg <- paste0("--library=", lib_location)
        }

        # Run R CMD INSTALL with appropriate settings
        args <- c("CMD", "INSTALL",
          lib_location_arg,
          "--no-docs",
          "--no-demo",
          "--no-help",
          "--no-multiarch",
          "--with-keep.source",
          "--pkglock",
          "--no-staged-install",
          normalizePath(file.path(folder, package))
        )

        system2(cmd, args, stdout = quiet, stderr = quiet)

      } else {
        # Run R CMD BUILD to zip file
        args <- c("CMD", "build", normalizePath(file.path(folder, package)))
        system2(cmd, args, stdout = quiet, stderr = quiet)
        pkg_file <- paste0(new_folder, .Platform$file.sep, package, "_", version, ".tar.gz")
        pkg_newfile <- paste0(getwd(), .Platform$file.sep, package, "_", version, ".tar.gz")
        if(file.exists(pkg_file)) {
          file.copy(pkg_file, pkg_newfile)
          message(paste0("Package built in: ", pkg_newfile))
        } else {
          message("Package not created.")
        }
      }
    }
    return()
  }


}
