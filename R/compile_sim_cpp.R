#' @export
compile_sim_cpp <- function(func, p, cpp_show_function) {
  folder <- c(system.file(package="PKPDsim"))
  test <- "\n  dAdt[0] = -VMAX1 * (A[0]/V) / (A[0]/V + KM) + rate ;\n"
  ode_def <- get(paste0(func, "_cpp"))
  ode_def_cpp <- reduce_state_numbers(ode_def)
  n <- names(p)
  p$rate <- 0
  pars <- "\n"
  par_def <- "\n"
  for(i in seq(n)) {
    pars <- paste0(pars, "double ", n[i], ";\n")
    par_def <- paste0('  ', par_def, n[i], ' = par["', n[i], '"];\n')
  }
  comp_def <- paste0("const double n_comp = ", attr(ode_def, "size"), ";\n",
                     "typedef boost::array < double , ", attr(ode_def, "size"), " > state_type; \n");
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
  if(cpp_show_function) {
    cat(sim_func)
  }
  sourceCpp(code=sim_func)
}
