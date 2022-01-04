#' Auto-detect the syntax for the ODE code
#'
#' Either PKPDsim or RxODE
#'
#' @param code character string with ODE code
#' @export
#' @return List with elements `from` and `to` indicating the syntax for the ODE
#'   code
#' @md
detect_ode_syntax <- function(code) {
  ## currently very simple, supports only PKPDsim / RxODE
  from <- "PKPDsim"
  to <- "RxODE"
  if(any(stringr::str_detect(tolower(code), "d/dt"))) {
    to <- "PKPDsim"
    from <- "RxODE"
  }
  return(list(from = from, to = to))
}
