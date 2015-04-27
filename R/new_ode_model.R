#' @export
new_ode_model <- function (model = NULL,
                           code = NULL,
                           state_init = NULL,
                           file = NULL,
                           parameters = NULL,
                           size = NULL,
                           obs = list("cmt" = 1, scale = 1),
                           dose = list("cmt" = 1),
                           declare_variables = NULL,
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
  code_init_text <- ""
  if(class(state_init) == "character") {
    code_init_text <- paste0(state_init, ";\n")
  }
  if(class(code) == "list") {
    code_tmp <- code
    code <- "" # write new code, combining all list elements
    n <- 0
    for(i in seq(names(code_tmp))) {
      idx <- names(code_tmp)[i]
      tmp <- code_tmp[[idx]]
      if (i > 1) {
        code_tmp[[idx]] <- shift_state_indices(code_tmp[[idx]], n)
        if(class(state_init) == "list" && !is.null(state_init[[idx]])) {
          code_init_text <- paste(code_init_text, shift_state_indices(unlist(state_init[[idx]]), n), ";\n");
        }
      }
      code <- paste(code, code_tmp[[idx]], sep = "\n")
      n <- n + get_ode_model_size(tmp)
    }
  }
  code_init_text <- shift_state_indices(code_init_text, -1)
  if(is.null(parameters)) {
    parameters <- get_parameters_from_code(code, declare_variables)
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
  code <- gsub("^;", "", code)
  compile_sim_cpp(code, size, parameters, cpp_show_code = cpp_show_code, code_init = code_init_text, declare_variables = declare_variables, verbose = verbose)
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
