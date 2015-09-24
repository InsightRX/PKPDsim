#' ODE system for PK - 1 compartment oral administration
#'
#' @param t time vector
#' @param A initial state vector
#' @param p parameters (list)
#'
#' @export
pk_1cmt_oral <- function (t, A, p) {
  with(p, {

    ## Parameter translation & calculations
    KEL <-  CL/V

    ## ODE definition:
    return(list(c(
      -KA*A[1] + rate,
      -KEL*A[2] + KA*A[1]
    )))
  })
}

## Indicate observation compartment and scaling:
attributes(pk_1cmt_oral) <- list(obs = list (cmt = 2, scale = "V"))
