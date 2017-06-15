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
  cat(paste0("Required parameters: ", paste(attr(x, "parameters"), collapse=", ")), "\n")
  cat(paste0("Covariates: ", paste(attr(x, "covariates"), collapse=", ")), "\n")
  cat(paste0("Variables: ", paste(attr(x, "variables"), collapse=", ")), "\n")
  cat(paste0("Number of compartments: ", attr(x, "size")), "\n")
  cat(paste0("Observation compartment: ", paste(attr(x, "obs")$cmt, collapse=", "), "\n"))
  cat(paste0("Observation scaling: ", paste(attr(x, "obs")$scale, collapse=", "), "\n"))
  cat(paste0("Lag time: ", attr(x, "lagtime")), "\n")
}
