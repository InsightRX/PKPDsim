#' ODE system for PK - 2 compartment IV
#'
#' @param t time vector
#' @param A initial state vector
#' @param p parameters (list)
#'
#' @export
pk_2cmt_iv <- function (t, A, p) {
  with(p, {

    ## Parameter translation & calculations:
    KEL <- CL/V
    K12 <- Q2/V
    K21 <- Q2/V2

    ## ODE definition
    return(list(c(
       -KEL*A[1] - K12*A[1] + K21*A[2] + rate,
       +K12*A[1] - K21*A[2]
    )))
  })
}
## Indicate observation compartment and scaling:
attributes(pk_2cmt_iv) <- list(obs = list (cmt = 1, scale = "V"))
