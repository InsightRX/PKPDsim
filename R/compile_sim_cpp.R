#' @export
compile_sim_cpp <- function(code, size, p, cpp_show_code, code_init = NULL, declare_variables = NULL, verbose = FALSE) {
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
  if(any(p %in% defined)) {
    p <- p[!p %in% defined]
  }
  p_def <- unique(p) # add rate and conc as explicitly declared variables
  p <- unique(c(p, "rate", "conc", declare_variables)) # add rate and conc as explicitly declared variables
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
  if (verbose) {
    sourceCpp(code=sim_func, rebuild = TRUE)
  } else {
    flg <- Sys.getenv("PKG_CXXFLAGS")
    if(length(grep("-w", flg)) == 0) {
      Sys.setenv("PKG_CXXFLAGS" = paste(flg, "-w"))
    }
    sourceCpp(code=sim_func, rebuild = TRUE, env = globalenv(), verbose = verbose, showOutput = verbose)
    Sys.setenv("PKG_CXXFLAGS" = flg)
  }
  return(list(parameters = get_parameters_from_code(ode_def_cpp, unique(c(defined, declare_variables)))))
}
