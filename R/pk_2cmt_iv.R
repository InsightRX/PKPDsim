#' ODE system for PK - 2 compartment IV
#'
#' @export
pk_2cmt_iv <- function (t, A, p) {
  with(p, {
    KEL <- CL/V
    K12 <- Q2/V
    K21 <- Q2/V2
    dAdt_1 <- -KEL*A[1] - K12*A[1] + K21*A[2] + rate
    dAdt_2 <- +K12*A[1] - K21*A[2]
    return ( list ( c (  dAdt_1, dAdt_2) ) )
  })
}
attributes(pk_2cmt_iv) <- list(obs = list (cmt = 1, scale = "V"))
