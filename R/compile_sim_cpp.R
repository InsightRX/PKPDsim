#' @export
compile_sim_cpp <- function(func, p) {
  folder <- c(system.file(package="PKPDsim"))
  cpp_file <- paste0(folder, "/cpp/", func, ".cpp")
  if(!file.exists(cpp_file)) {
    stop("Specified C++ ODE function not found!")
  } else {
    n <- names(p)
    pars <- ""
    for(i in seq(n)) {
      pars <- paste0(pars, "double ", n[i], ";\n")
    }
    cpp_code <- readLines(paste0(folder, "/cpp/sim.cpp"))
    par_def <- 'KA = par["KA"];\n    CL = par["CL"];\n    V = par["V"];'
    idx <- grep("insert_parameter_definitions", cpp_code)
    cpp_code[idx] <- par_def
    sim_func <-
      paste0(paste0(readLines(paste0(folder, "/cpp/sim_header.cpp")), collapse = "\n"),
             pars,
             paste0(readLines(cpp_file), collapse="\n"),
             paste0(cpp_code, collapse = "\n"))
    sourceCpp(code=sim_func)
  }
}
