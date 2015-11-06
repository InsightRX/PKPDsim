#' Print function for PKPDsim simulation function
#'
#' @param x function
#' @export
print.PKPDsim <- function(x) {
  cat(paste0("ODE definition: \n", attr(x, "code")), "\n")
  cat(paste0("Required parameters: ", paste(attr(x, "parameters"), collapse=", ")), "\n")
  cat(paste0("Number of compartments: ", attr(x, "size")), "\n")
  cat(paste0("Observation compartment: ", attr(x, "obs")$cmt), "\n")
  cat(paste0("Dependent variable scaling: ", attr(x, "obs")$scale), "\n")
}
