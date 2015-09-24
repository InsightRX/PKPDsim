## R version #################################################################
#' ODE system for PK - 1 compartment IV
#'
#' @param t time vector
#' @param A initial state vector
#' @param p parameters (list)
#' @export
pk_1cmt_iv <- function (t, A, p) {
  with(p, {

    ## Parameter translation & calculations
    KEL <-  CL/V

    ## ODE definition:
    return(list(c(
      -KEL*A[1] + rate
    )))
  })
}
## Indicate observation compartment and scaling:
attributes(pk_1cmt_iv) <- list(obs = list (cmt = 1, scale = "V"))
