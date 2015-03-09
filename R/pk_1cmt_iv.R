#' ODE system for PK - 1 compartment IV
#'
#' @export
pk_1cmt_iv <- function (t, A, p) {
  with(p, {
    KEL <-  CL/V
    dAdt_1 <- -KEL*A[1] + rate
    return ( list ( c (  dAdt_1 ) ) )
  })
}
attributes(pk_1cmt_iv) <- list(obs = list (cmt = 1, scale = "V"))
