#' @export
compile_sim_cpp <- function(code, size, p, cpp_show_code, verbose = FALSE) {
  folder <- c(system.file(package="PKPDsim"))
  ode_def <- code
  ode_def_cpp <- reduce_state_numbers(ode_def)
  n <- names(p)
  p$rate <- 0
  pars <- "\n"
  par_def <- "\n"
  for(i in seq(n)) {
    pars <- paste0(pars, "double ", n[i], ";\n")
    par_def <- paste0('  ', par_def, n[i], ' = par["', n[i], '"];\n')
  }
  comp_def <- paste0("const double n_comp = ", size, ";\n",
                     "typedef boost::array < double , ", size, " > state_type; \n");
  cpp_code <- readLines(paste0(folder, "/cpp/sim.cpp"))
  idx <- grep("insert_parameter_definitions", cpp_code)
  cpp_code[idx] <- par_def
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
}
