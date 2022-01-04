#' Test if model still in memory
#' @param model pointer to model
#' @export
#' @return No return value
test_pointer <- function(model) {
  tmp <- utils::capture.output(model)
  if(length(grep("0x0>", tmp[2])) > 0) {
    stop("Model not in memory anymore. Please recompile using new_ode_model().")
  }
}
