#' Remove n doses (from tail) of PKPDsim regimen
#'
#' Opposite of shift_regimen()
#'
#' @param regimen PKPDsim regimen created using `new_regimen()`
#' @param n number of doses to pop from regimen
#'
#' @seealso shift_regimen
#' @export
#' @return Input regiment minus selected number of doses
pop_regimen <- function(
    regimen,
    n = 1) {
    if(n >= length(regimen$dose_amts)) {
        return(NULL)
    }
    idx <- 1:(length(regimen$dose_amts)-n)
    regimen$dose_amts <- regimen$dose_amts[idx]
    regimen$dose_times <- regimen$dose_times[idx]
    regimen$type <- regimen$type[idx]
    regimen$t_inf <- regimen$t_inf[idx]
    regimen$n <- length(idx)
    return(regimen)
}
