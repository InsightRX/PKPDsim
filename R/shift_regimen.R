#' Remove n doses (from start) of PKPDsim regimen
#'
#' Opposite of pop_regimen()
#' 
#' @param regimen PKPDsim regimen created using `new_regimen()`
#' @param n number of doses to shift regimen
#' @param reset_time reset the remaining doses to start at t=0?
#'
#' @seealso pop_regimen
#' @export
#' @return Regimen with selected number of doses removed from start
shift_regimen <- function(
    regimen,
    n = 1,
    reset_time = TRUE) {
    regimen$dose_amts <- regimen$dose_amts[-(1:n)]
    regimen$dose_times <- regimen$dose_times[-(1:n)]
    if(reset_time) {
      regimen$dose_times <- regimen$dose_times - regimen$dose_times[1]
    }
    regimen$type <- regimen$type[-(1:n)]
    regimen$t_inf <- regimen$t_inf[-(1:n)]
    regimen$n <- length(regimen$dose_amts)
    if(regimen$n == 0) {
        return(NULL)
    }
    return(regimen)
}
