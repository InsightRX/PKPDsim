#' @export
compile_sim_cpp <- function(func) {
  folder <- c(system.file(package="PKPDsim"))
  cpp_file <- paste0(folder, "/cpp/", func, ".cpp")
  if(!file.exists(cpp_file)) {
    stop("Specified C++ ODE function not found!")
  } else {
    sim_func <-
      paste0(paste0(readLines(paste0(folder, "/cpp/sim_header.cpp")), collapse = "\n"),
             paste0(readLines(cpp_file), collapse = "\n"),
             paste0(readLines(paste0(folder, "/cpp/sim.cpp")), collapse = "\n"))
    sourceCpp(code=sim_func)
  }
}
