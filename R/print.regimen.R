#' Print function for PKPDsim regimen
#'
#' @param x regimen
#' @param ... arguments to pass
#' @return No return value, print function.
#' @export
#' @keywords internal
print.regimen <- function(x, ...) {
  if(!is.null(x) && !is.null(x$dose_times) && length(x$dose_times) > 0) {
    tmp <- data.frame(cbind(t = x$dose_times, type = x$type, amt = x$dose_amts, cmt = x$cmt))
    if(!is.null(x$t_inf)) {
      tmp <- data.frame(cbind(tmp, t_inf = x$t_inf))
    }
    if(!is.null(x$ss_regimen)) {
      message("Note: Steady state regimen, steady state assumed to be reached at t = 0.\n")
    }
    print(tmp)
  } else {
    print("Not a PKPDSim regimen structure.")
  }
}
