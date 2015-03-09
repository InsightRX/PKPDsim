#' ODE system for PK - 1 compartment oral administration
#'
#' @export
pk_1cmt_oral <- function (t, A, p) {
  with(p, {
    KEL <-  CL/V
    dAdt_1 <- -KA*A[1]
    dAdt_2 <- -KEL*A[2] + KA*A(1)
    return ( list ( c (  dAdt_1, dAdt_2 ) ) )
  })
}
attributes(pk_1cmt_oral) <- list(obs = list (cmt = 2, scale = "V"))
