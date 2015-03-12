#' ODE system for PK - 2 compartment oral administration
#'
#' @export
pk_2cmt_oral <- function (t, A, p) {
  with(p, {
    KEL <- CL/V
    K12 <- Q2/V
    K21 <- Q2/V2
    dAdt_1 <- -KA*A[1] + rate
    dAdt_2 <- -KEL*A[1] - K12*A[1] + K21*A[2] + KA*A[1]
    dAdt_3 <- +K12*A[1] - K21*A[2]
    return ( list ( c (  dAdt_1, dAdt_2, dAdt_3 ) ) )
  })
}
attributes(pk_2cmt_oral) <- list(obs = list (cmt = 2, scale = "V"))
