#' Compile ODE model to c++ function
#'
#' @param code C++ code ODE system
#' @param dose_code C++ code per dose event
#' @param pk_code C++ code per any event (similar to $PK)
#' @param size size of ODE system
#' @param p parameters (list)
#' @param cpp_show_code show output c++ function?
#' @param code_init code for initialization of state
#' @param state_init state init vector
#' @param declare_variables variable declaration for all required variables (including user-specified)
#' @param variables only the user-specified variables
#' @param covariates covariates specification
#' @param obs observation specification
#' @param dose dose specification
#' @param iov iov specification
#' @param compile compile or not?
#' @param verbose show more output
#' @param as_is use C-code as-is, don't substitute line-endings or shift indices
#' @export
#' @return List containing ODE definition in C++ code and simulation function
compile_sim_cpp <- function(
  code,
  dose_code,
  pk_code,
  size,
  p,
  cpp_show_code,
  code_init = NULL,
  state_init = NULL,
  declare_variables = NULL,
  variables = NULL,
  covariates = NULL,
  obs = NULL,
  dose = NULL,
  iov = NULL,
  compile = TRUE,
  verbose = FALSE,
  as_is = FALSE) {

  folder <- c(system.file(package="PKPDsim"))
  ode_def <- code

  ## find newly declared variables and make sure they are defined as double
  ode_def <- paste0(gsub("[\n^]( *?)double ", "", ode_def))
  newpar <- gregexpr("[\n^](.*?)=", ode_def)
  #
#  par1 <- regmatches(par1, newpar)[[1]]
  pars <- unlist(strsplit(ode_def, "\n"))
  par1 <- c()
  for(pr in pars) {
    if(length(grep("=", pr))>0) {
      par1 <- c(par1, gsub("=.*$", "=", pr))
    }
  }
  def1 <- par1[grep("\\=", par1)]
  def1 <- par1[-grep("dadt\\[", tolower(def1))]
  if(length(grep("\\(", def1)) > 0) {
    def1 <- def1[-grep("\\(", def1)]
  }
  for (i in seq(def1)) {
    tmp <- gsub("[\n =;]*", "", def1[i])
    if(tmp %in% declare_variables) { # is already defined as global variable
      ode_def <- gsub(def1[i], paste0("\n", gsub("\n[;]*","",def1[i])), ode_def, perl=TRUE)
    } else {
      ode_def <- gsub(def1[i], paste0("\ndouble ", gsub("\n[;]*?","",def1[i])), ode_def, perl=TRUE)
    }
  }
  par1 <- gsub("[\n\\= ]", "", unique(par1))
  par1 <- gsub("double ", "", par1)
  par1 <- gsub(";", "", par1)
  defined <- par1[-grep("dadt\\[", tolower(par1))]
  if(!as_is) {
    ode_def_cpp <- shift_state_indices(ode_def, -1)
    ode_def_cpp <- gsub("\\n *", "\\\n  ", ode_def_cpp)
  } else {
    ode_def_cpp <- ode_def
  }

  # add 'rate' for dose compartments to allow infusions, remove if already specified by user (previous versions req'd this)
  if(is.null(dose)) {
    dose <- list(cmt = 1)
  }
  line_end <- gregexpr(";", ode_def_cpp)[[1]]
  match_rate <- gregexpr(paste0("\\+(.)*rate"), tolower(ode_def_cpp))[[1]]
  if(match_rate[1] >= 0) {
    stop("Sorry, the manual specification of `rate` in the ODE definition is deprecated. Please use the `dose` argument in `new_ode_model()`, or the `cmt` argument in `new_regimen()` instead.")
  }
  lines <- strsplit(ode_def_cpp, ";")[[1]]
  new_ode <- c()
  for(i in seq(lines)) {
    if(length(grep("dAdt", lines[i])) > 0) {
      m <- gregexpr("dAdt\\[([0-9]*)\\]", lines[i], perl=TRUE)
      start <- unlist(attr(m[[1]], "capture.start"))[,1]
      len <- attr(m[[1]], "capture.length")
      num <- substr(lines[i], start, start+len-1)
      lines[i] <- paste0(lines[i], " + rate[", num, "]")
    }
    if(nchar(lines[i]) > 0) {
      new_ode <- c(new_ode, lines[i])
    }
  }
  ode_def_cpp <- paste(new_ode, collapse=";")

  if(any(p %in% defined)) {
    p <- p[!p %in% defined]
  }
  p_def <- unique(p)
  if (!is.null(declare_variables)) {
    m <- p_def %in% declare_variables # remove covariates and other declared variables
    p_def <- p_def[!m]
  }
  if(!is.null(obs) && length(obs$scale) > 1) {
    p <- c(p, paste0("scale", 1:length(obs$scale)))
  } else {
    p <- c(p, "scale")
  }
  p <- unique(c(p, "conc", declare_variables)) # add rate and conc as explicitly declared variables
  pars <- "\n"
  par_def <- ""
  for(i in seq(p)) { # parameters and auxiliary variables
    pars <- paste0(pars, "double ", p[i], ";\n")
  }
  pars <- paste0(pars, "double prv_dose, t_prv_dose = 0;\n")
  ## Initialize rate and bioav variables and insert temporary dummy values.
  pars <- paste0(pars, paste0("double rate[] = { ", paste(rep(0, size), collapse=", "), " };\n"))
  if(length(dose$bioav) < size) {
    dose$bioav <- c(dose$bioav, rep(1, size - length(dose$bioav)))
  }
  pars <- paste0(pars, paste0("double bioav[] = { ", paste(rep(1, size), collapse=", "), " };\n"))
  if(!is.null(iov) && !is.null(iov$n_bins)) {
    pars <- paste0(pars, paste0("Rcpp::NumericVector iov_bin(", (iov$n_bins+1) ,");\n"))
    par_def <- paste0('  for(int i = 0; i < (iov_bin.size()); i++) { iov_bin[i] = iov_bins[i]; };\n', par_def);
  }
  for(i in seq(p_def)) { # actual parameters for model
    par_def <- paste0(par_def, '  ', p_def[i], ' = par["', p_def[i], '"];\n')
  }
  comp_def <- paste0("const int n_comp = ", size, ";\n",
                     "typedef boost::array < double , ", size, " > state_type; \n");
  cpp_code <- readLines(paste0(folder, "/cpp/sim.cpp"))
  idx <- grep("insert_parameter_definitions", cpp_code)
  cpp_code[idx] <- par_def
  idx2 <- grep("insert_state_init", cpp_code)
  cpp_code[idx2] <- paste("   ", code_init)
  cov_scale <- ""
  cov_names <- NULL
  if(!is.null(covariates)) {
    if(inherits(covariates, "character")) {
      cov_names <- covariates
    }
    if(inherits(covariates, "list")) {
      cov_names <- names(covariates)
    }
  }
  if(!is.null(cov_names) && length(cov_names) > 0) {
    cov_def <- "  // covariate definitions\n"
    cov_tmp <- "    // covariates during integration period\n"
    for(i in seq(cov_names)) {
      nam <- cov_names[i]
      ode_def_cpp <- paste0(
        paste0('    ', nam, ' = ', nam, '_0 + gr_', nam, ' * (t - t_prv_', nam, ');\n'),
        ode_def_cpp)
      pars <- paste0(pars, paste0("Rcpp::NumericVector cov_", nam, ";\n"))
      pars <- paste0(pars, paste0("Rcpp::NumericVector cov_t_", nam, ";\n"))
      pars <- paste0(pars, paste0("Rcpp::NumericVector gradients_", nam, ";\n"))
      cov_def <- paste0(cov_def, paste0('  cov_', nam, ' = design["cov_', nam,'"];\n'))
      cov_def <- paste0(cov_def, paste0('  cov_t_', nam, ' = design["cov_t_', nam,'"];\n'))
      cov_def <- paste0(cov_def, paste0('  gradients_', nam, ' = design["gradients_', nam,'"];\n'))
      cov_def <- paste0(cov_def, paste0('  ', nam,' = cov_', nam,'[0];\n'))
      cov_tmp <- paste0(cov_tmp, paste0('    ', nam, '_0 = cov_', nam,'[i];\n'))
      if(inherits(covariates, "character") || (inherits(covariates, "list") && tolower(covariates[[nam]]$implementation) != "locf")) {
        cov_tmp <- paste0(cov_tmp, paste0('    gr_', nam, ' = gradients_',nam,'[i] ;\n'))
        cov_tmp <- paste0(cov_tmp, paste0('    t_prv_', nam, ' = cov_t_', nam, '[i] ;\n'))
      } else { ## if covariates specified as character vector, also assume non-timevarying
        cov_tmp <- paste0(cov_tmp, paste0('    gr_', nam, ' = 0 ;\n'))
        cov_tmp <- paste0(cov_tmp, paste0('    t_prv_', nam, ' = 0 ;\n'))
      }
      cov_tmp <- paste0(cov_tmp, paste0('    ', nam, ' = ', nam,'_0;\n'))
      cov_scale <- paste0(cov_scale, paste0('      ', nam, ' = ', nam, '_0 + gr_', nam, ' * (tmp.time[k] - t_prv_', nam), ');\n')
    }
    idx3 <- grep("insert covariate definitions", cpp_code)
    cpp_code[idx3] <- cov_def
    idx4 <- grep("insert covariates for integration period", cpp_code)
    cpp_code[idx4] <- cov_tmp
  }
  idx5 <- grep("insert scale definition for integration period", cpp_code)
  idx6 <- grep("observation compartment", cpp_code)
  idx7 <- grep("insert scale definition for observation", cpp_code)
  idx8 <- grep("insert time-dependent covariates scale", cpp_code)
  idx9 <- grep("insert custom pk event code", cpp_code)
  idx10 <- grep("insert bioav definition", cpp_code)
  idx11 <- grep("insert custom dosing event code", cpp_code)
  idx12a <- grep("insert saving initial observations", cpp_code)
  idx12b <- grep("insert saving loop observations", cpp_code)
  idx13 <- grep("insert copy observation object", cpp_code)
  idx14 <- grep("insert observation variable definition", cpp_code)
  idx15 <- grep("insert copy variables", cpp_code)
  idx16 <- grep("insert copy all variables", cpp_code)
  idx17 <- grep("insert variable definitions", cpp_code)
  idx18 <- grep("insert A dAdt state_init", cpp_code)
  if(is.null(obs)) {
    cpp_code[idx5] <- "    double scale = 1;"
    cpp_code[idx6] <- "  int cmt = 0;"
    cpp_code[idx7] <- "  scale = 1;"
    cpp_code[idx8] <- paste0("    scale = ", obs$scale[1], ";")
    cpp_code[idx14] <- "  std::vector<double> obs;"
  } else {
    if(length(obs$cmt) == 1) {
      cpp_code[idx5] <- paste0(cpp_code[idx5], "\n    scale = ", obs$scale[1], ";")
      cpp_code[idx7] <- paste0(cpp_code[idx7], "\n      scale = ", obs$scale[1], ";")
      cpp_code[idx8] <- cov_scale
      cpp_code[idx12a] <- paste0(cpp_code[idx12a], "\n      obs.insert(obs.end(), tmp.y[k][", obs$cmt[1]-1,"] / scale);")
      cpp_code[idx12b] <- paste0(cpp_code[idx12b], "\n      obs.insert(obs.end(), tmp.y[k][", obs$cmt[1]-1,"] / scale);")
      cpp_code[idx13] <- paste0('  comb["obs"] = obs;\n');
      cpp_code[idx14] <- "  std::vector<double> obs;"
    } else {
      if(is.null(obs$cmt)) obs$cmt <- 1
      for(k in 1:length(obs$cmt)) {
        cpp_code[idx5] <- paste0(cpp_code[idx5], "\n    scale", k," = ", obs$scale[k], ";")
        cpp_code[idx7] <- paste0(cpp_code[idx7], "\n      scale", k," = ", obs$scale[k], ";")
        cpp_code[idx8] <- cov_scale
        cpp_code[idx12a] <- paste0(cpp_code[idx12a], "\n      obs",k,".insert(obs",k,".end(), tmp.y[k][", obs$cmt[k]-1,"] / scale", k,");")
        cpp_code[idx12b] <- paste0(cpp_code[idx12b], "\n      obs",k,".insert(obs",k,".end(), tmp.y[k][", obs$cmt[k]-1,"] / scale", k,");")
        cpp_code[idx13] <- paste0(cpp_code[idx13], '\n  comb["obs', k,'"] = obs', k,';');
        cpp_code[idx14] <- paste0(cpp_code[idx14], "\n  std::vector<double> obs",k,";")
      }
    }
    if(!is.null(obs$variable) && length(obs$variable) > 0) {
      cpp_code[idx12a] <- parse_obs_types(obs, initial = TRUE)
      cpp_code[idx12b] <- parse_obs_types(obs, initial = FALSE)
    }
  }
  if(!is.null(variables)) {
    cpp_code[idx17] <- paste0("  std::vector<double> ", paste(paste0("vars_", variables), collapse = ", "), ";");
    for(k in seq(variables)) {
      cpp_code[idx15] <- paste0(cpp_code[idx15], '\n      vars_',variables[k],'.insert(vars_', variables[k],'.end(), ', declare_variables[k],");")
      cpp_code[idx16] <- paste0(cpp_code[idx16], '\n  comb["', variables[k],'"] = vars_', variables[k],";")
    }
  }
  if(!is.null(pk_code)) {
    cpp_code[idx9] <- shift_state_indices(pk_code, -1)
  }
  if(!is.null(dose$bioav)) {
    not1 <- dose$bioav != 1 # keep C++ code clean and only redeclare F when not 1
    if(any(not1)) {
      for(i in (1:size)[not1]) {
        cpp_code[idx10] <- paste(paste0("    bioav[", (1:size)[not1]-1, "] = ", dose$bioav[not1], ";"), collapse = "\n")
      }
    }
  }
  if(!is.null(dose_code)) {
    cpp_code[idx11] <- shift_state_indices(dose_code, -1)
  }
  cpp_code[idx18] <- paste0(
    "  boost::array<double, ",size,"> b = { ", paste(rep(0, size), collapse=", ")," };\n",
    "  const state_type& A_dum = b;\n",
    "  state_type dAdt_dum = b;")
  sim_func <-
    paste0(paste0(readLines(paste0(folder, "/cpp/sim_header.cpp")), collapse = "\n"),
           pars,
           comp_def,
           "\nvoid ode ( const state_type &A , state_type &dAdt , double t ) {\n",
           ode_def_cpp,
           "\n}\n\n",
           paste0(cpp_code, collapse = "\n"))
  if(cpp_show_code) {
    cat(sim_func)
  }
  flg <- Sys.getenv("PKG_CXXFLAGS")
  if(length(grep("-w", flg)) == 0) {
    Sys.setenv("PKG_CXXFLAGS" = paste(flg, "-w"))
  }
  if(compile) {
    Rcpp::sourceCpp(code=sim_func, rebuild = TRUE, env = globalenv(), verbose = verbose, showOutput = verbose)
    Sys.setenv("PKG_CXXFLAGS" = flg)
  }
  return(list(
    ode = ode_def_cpp,
    cpp = sim_func
  ))
}
