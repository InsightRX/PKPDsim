#' ODE system for PK - 3 compartment IV
#'
#' @export
pk_3cmt_iv <- function (t, A, p) {
  with(p, {
    KEL <- CL/V
    K12 <- Q2/V
    K21 <- Q2/V2
    K13 <- Q3/V
    K31 <- Q3/V3
    dAdt_1 <- -KEL*A[1] - K12*A[1] + K21*A[2] - K13*A[1] + K31*A[3] + rate
    dAdt_2 <- +K12*A[1] - K21*A[2]
    dAdt_3 <- +K13*A[1] - K31*A[3]
    return ( list ( c (  dAdt_1, dAdt_2, dAdt_3) ) )
  })
}
attributes(pk_3cmt_iv) <- list(size = 3, obs = list (cmt = 1, scale = "V"))
