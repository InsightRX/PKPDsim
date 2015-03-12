#' ODE system for PK - 2 compartment oral administration
#'
#' @export
pk_2cmt_oral <- function (t, A, p) {
  with(p, {

    ## Parameter translation & calculations:
    KEL <- CL/V
    K12 <- Q2/V
    K21 <- Q2/V2

    ## ODE definitions
    return(list(c(
      -KA*A[1] + rate,
      -KEL*A[1] - K12*A[1] + K21*A[2] + KA*A[1],
      +K12*A[1] - K21*A[2]
    )))
  })
}
## Indicate observation compartment and scaling:
attributes(pk_2cmt_oral) <- list(obs = list (cmt = 2, scale = "V"))
