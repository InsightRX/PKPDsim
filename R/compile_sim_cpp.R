#' @export
compile_sim_cpp <- function(func, p) {
  folder <- c(system.file(package="PKPDsim"))
  cpp_file <- paste0(folder, "/cpp/", func, ".cpp")
  if(!file.exists(cpp_file)) {
    stop("Specified C++ ODE function not found!")
  } else {
    n <- names(p)
    p$rate <- 0
    pars <- "\n"
    par_def <- "\n"
    for(i in seq(n)) {
      pars <- paste0(pars, "double ", n[i], ";\n")
      par_def <- paste0('  ', par_def, n[i], ' = par["', n[i], '"];\n')
    }
    cpp_code <- readLines(paste0(folder, "/cpp/sim.cpp"))
    idx <- grep("insert_parameter_definitions", cpp_code)
    cpp_code[idx] <- par_def
    sim_func <-
      paste0(paste0(readLines(paste0(folder, "/cpp/sim_header.cpp")), collapse = "\n"),
             pars,
             paste0(readLines(cpp_file), collapse="\n"),
             paste0(cpp_code, collapse = "\n"))
#     cat(sim_func)
    sourceCpp(code=sim_func)
  }
}
