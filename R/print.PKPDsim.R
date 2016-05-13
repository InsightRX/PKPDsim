#' Print function for PKPDsim simulation function
#'
#' @param x function
#' @param ... additional arguments
#' @export
print.PKPDsim <- function(x, ...) {
  cat(paste0("ODE definition: \n", attr(x, "code")), "\n")
  cat(paste0("Required parameters: ", paste(attr(x, "parameters"), collapse=", ")), "\n")
  cat(paste0("Covariates: ", paste(attr(x, "covariates"), collapse=", ")), "\n")
  cat(paste0("Variables: ", paste(attr(x, "variables"), collapse=", ")), "\n")
  cat(paste0("Number of compartments: ", attr(x, "size")), "\n")
  cat(paste0("Observation compartment: ", attr(x, "obs")$cmt), "\n")
  cat(paste0("Observation scaling: ", attr(x, "obs")$scale), "\n")
  if(!is.null(attr(x, "lagtime"))) {
    cat(paste0("Lag time(s): [ ", paste(attr(x, "lagtime"), collapse=", "), " ]"), "\n")
  }
}
