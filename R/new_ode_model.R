#' @export
new_ode_model <- function (model = NULL,
                           code = NULL,
                           file = NULL,
                           parameters = NULL,
                           size = NULL,
                           obs = list("cmt" = 1, scale = 1),
                           dose = list("cmt" = 1),
                           cpp_show_code = FALSE,
                           verbose = FALSE) {
  if (is.null(model) & is.null(code) & is.null(file)) {
    stop(paste0("Either a model name, ODE code, or a file containing code for the ODE system have to be supplied to this function. The following models are availabler in the default library:\n  ", model_library()))
  }
  if (!is.null(model)) {
    mod_def <- model_library(model)
    code <- mod_def$code
    obs <- mod_def$obs
    dose <- mod_def$dose
  }
  if(is.null(parameters)) {
    parameters <- get_parameters_from_code(code)
  }
  if(exists("sim_wrapper_cpp", envir = globalenv())) {
    rm("sim_wrapper_cpp", envir=globalenv())
  }
  if(is.null(size)) {
    size <- get_ode_model_size(code)
  }
  code_orig <- code
  code <- gsub("\\r\\n", "\n", code)
  code <- gsub("\\n", ";\n", code)
  compile_sim_cpp(code, size, parameters, cpp_show_code = cpp_show_code, verbose = verbose)
  attr(sim_wrapper_cpp, "size")  <- size
  attr(sim_wrapper_cpp, "obs")  <- obs
  attr(sim_wrapper_cpp, "dose") <- dose
  attr(sim_wrapper_cpp, "parameters") <- parameters
  attr(sim_wrapper_cpp, "cpp")  <- TRUE
  attr(sim_wrapper_cpp, "code") <- code_orig
  if(exists("sim_wrapper_cpp", envir = globalenv())) {
    return(sim_wrapper_cpp)
  } else {
    message("Compilation failed. Please use verbose=TRUE and cpp_show_code=TRUE arguments to debug.")
  }
}
