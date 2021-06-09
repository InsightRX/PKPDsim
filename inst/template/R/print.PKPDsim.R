#' Print function for PKPDsim simulation function
#'
#' @param x function
#' @param ... additional arguments
#' @export
print.PKPDsim <- function(x, ...) {
  cat(paste0("ODE definition: \n", attr(x, "code")), "\n")
  if(!is.null(attr(x, "pk_code")) && attr(x, "pk_code") != "") {
    cat(paste0("PK event code: \n", attr(x, "pk_code")), "\n")
  }
  if(!is.null(attr(x, "state_init"))) {
    cat(paste("State init: \n", attr(x, "state_init"), "\n"))
  }
  cat(paste0("Required parameters: ", paste(attr(x, "parameters"), collapse=", ")), "\n")
  if(!is.null(attr(x, "mixture"))) {
    cat(paste0("Mixture model (", names(attr(x, "mixture")),"): \n", 
      "  Parameter values: ", paste(attr(x, "mixture")[[1]]$values, collapse=", "), "\n",
      "  Probabilities: ", paste(c(attr(x, "mixture")[[1]]$probability, 1-attr(x, "mixture")[[1]]$probability), collapse=", "), 
      "\n"))
  }
  cat(paste0("Covariates: ", paste(attr(x, "covariates"), collapse=", ")), "\n")
  cat(paste0("Variables: ", paste(attr(x, "variables"), collapse=", ")), "\n")
  cat(paste0("Fixed parameters: ", paste(attr(x, "fixed"), collapse=", ")), "\n")
  cat(paste0("Number of compartments: ", attr(x, "size")), "\n")
  if(!is.null(paste(attr(x, "obs")$variable))) {
    cat(paste0("Observation variable: ", paste(attr(x, "obs")$variable, collapse=", "), "\n"))
  } else {
    cat(paste0("Observation compartment: ", paste(attr(x, "obs")$cmt, collapse=", "), "\n"))
  }
  cat(paste0("Observation scaling: ", paste(attr(x, "obs")$scale, collapse=", "), "\n"))
  cat(paste0("Lag time: ", attr(x, "lagtime")), "\n")
  if(!is.null(attr(x, "ltbs")) && attr(x, "ltbs")) cat(paste("Transform: LTBS", "\n"))
  if(!is.null(attr(x, "int_step_size"))) cat(paste("ODE step size:", attr(x, "int_step_size"), "\n"))
  if(!is.null(attr(x, "iov")) && !is.null(attr(x, "iov")$n_bins)) {
    cat(paste0("IOV CV: ", jsonlite::toJSON(attr(x, "iov")$cv), "\n"))
    cat(paste0("IOV bins: ", attr(x, "iov")$n_bins, "\n"))
  }
  if(!is.null(attr(x, "comments"))) {
    cat(paste0("Comments: ", attr(x, "comments")))
  }
}
