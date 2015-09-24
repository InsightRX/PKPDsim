#' Compile ODE model to c++ function
#' @param code C++ code
#' @param size size of ODE system
#' @param p parameters (list)
#' @param cpp_show_code show output c++ function?
#' @param code_init code for initialization of state
#' @param declare_variables variable declaration
#' @param covariates covariates specification
#' @param obs observation specification
#' @param verbose show more output
#' @export
compile_sim_cpp <- function(code, size, p, cpp_show_code, code_init = NULL, declare_variables = NULL, covariates = NULL, obs = NULL, verbose = FALSE) {
  folder <- c(system.file(package="PKPDsim"))
  ode_def <- code

  ## find newly declared variables and make sure they are defined as double
  ode_def <- paste0("\n", gsub("[\n^]( *?)double ", "", ode_def))
  newpar <- gregexpr("[\n^](.*?)=", ode_def)
  par1 <- regmatches(ode_def, newpar)[[1]]
  def1 <- par1[-grep("dadt\\[", tolower(par1))]
  for (i in seq(def1)) {
    ode_def <- gsub(def1[i], paste0("\ndouble ", gsub("\n","",def1[i])), ode_def)
  }
  par1 <- gsub("[\n\\= ]", "", par1)
  par1 <- gsub("double ", "", par1)
  defined <- par1[-grep("dadt\\[", tolower(par1))]

  ode_def_cpp <- shift_state_indices(ode_def, -1)
  ode_def_cpp <- gsub("\\n *", "\\\n  ", ode_def_cpp)
  if(any(p %in% defined)) {
    p <- p[!p %in% defined]
  }
  p_def <- unique(p)
  if (!is.null(declare_variables)) {
    m <- p_def %in% declare_variables # remove covariates and other declared variables
    p_def <- p_def[!m]
  }
  p <- unique(c(p, "rate", "conc", "scale", declare_variables)) # add rate and conc as explicitly declared variables
  pars <- "\n"
  par_def <- ""
  for(i in seq(p)) { # parameters and auxiliary variables
    pars <- paste0(pars, "double ", p[i], ";\n")
  }
  for(i in seq(p_def)) { # actual parameters for model
    par_def <- paste0(par_def, '  ', p_def[i], ' = par["', p_def[i], '"];\n')
  }
  comp_def <- paste0("const double n_comp = ", size, ";\n",
                     "typedef boost::array < double , ", size, " > state_type; \n");
  cpp_code <- readLines(paste0(folder, "/cpp/sim.cpp"))
  idx <- grep("insert_parameter_definitions", cpp_code)
  cpp_code[idx] <- par_def
  idx2 <- grep("insert_state_init", cpp_code)
  cpp_code[idx2] <- paste("   ", code_init)
  if(!is.null(covariates)) {
    cov_def <- "  // covariate definitions\n"
    cov_tmp <- "    // covariates during integration period\n"
    for(i in seq(names(covariates))) {
      cov_def <- paste0(cov_def, paste0('  std::vector<double> cov_', names(covariates)[i], ' = design["cov_', names(covariates)[i],'"];\n'))
      cov_tmp <- paste0(cov_tmp, paste0('    ', names(covariates)[i], ' = cov_', names(covariates)[i],'[i];\n'))
    }
    idx3 <- grep("insert covariate definitions", cpp_code)
    cpp_code[idx3] <- cov_def
    idx4 <- grep("insert covariates for integration period", cpp_code)
    cpp_code[idx4] <- cov_tmp
  }
  idx5 <- grep("insert scale definition for integration period", cpp_code)
  idx6 <- grep("observation compartment", cpp_code)
  if(is.null(obs)) {
    cpp_code[idx5] = "    double scale = 1;"
    cpp_code[idx6] = "  int cmt = 0;"
  } else {
    cpp_code[idx5] = paste0("    scale = ", obs$scale, ";")
    cpp_code[idx6] = paste0("  int cmt = ", (obs$cmt-1), ";")
  }
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
  sourceCpp(code=sim_func, rebuild = TRUE, env = globalenv(), verbose = verbose, showOutput = verbose)
  Sys.setenv("PKG_CXXFLAGS" = flg)
  return(list(parameters = get_parameters_from_code(ode_def_cpp, unique(c(defined, declare_variables)))))
}
