#' ODE system for PK - 3 compartment IV
#'
#' @param t time vector
#' @param A initial state vector
#' @param p parameters (list)
#'
#' @export
pk_3cmt_iv <- function (t, A, p) {
  with(p, {

    ## Parameter translation & calculations
    KEL <- CL/V
    K12 <- Q2/V
    K21 <- Q2/V2
    K13 <- Q3/V
    K31 <- Q3/V3

    ## ODE definition:
    return(list(c(
       -KEL*A[1] - K12*A[1] + K21*A[2] - K13*A[1] + K31*A[3] + rate,
       +K12*A[1] - K21*A[2],
       +K13*A[1] - K31*A[3]
    )))
  })
}

## Indicate observation compartment and scaling:
attributes(pk_3cmt_iv) <- list(size = 3, obs = list (cmt = 1, scale = "V"))
