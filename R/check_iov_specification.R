#' Checks that IOV was specified appropriately
#'
#' Inter-occasion variability (IOV) is expected to be supplied as a list with
#' `cv` and `n_bins` specified. `cv` is expected to be a named list with IOV
#' for each PK parameter. This function then checks to ensure that the PK code
#' or ODE code contains an IOV term for each PK parameter specified.
#'
#' @param iov IOV specifications, provided as a nested named list.
#' @param code C++ ODE code, supplied as a string
#' @param pk_code C++ PK code, supplied as a string

check_iov_specification <- function(iov, code, pk_code){
  if(is.null(iov$cv) || class(iov$cv) != "list" || is.null(iov$n_bins)) {
    stop("IOV misspecified.")
  }
  lapply(
    names(iov$cv),
    function(x) {
      kappa_x <- paste0("kappa_", x)
      kappa_in_code <- any(grepl(kappa_x, code)) || any(grepl(kappa_x, pk_code))
      if (!kappa_in_code){
        message(paste(
            "IOV requested for parameter", x, "but no", kappa_x,
            "found in ODE or PK code. Please see documentation for more info."
        ))
      }
    }
  )
  invisible(NULL)
}
