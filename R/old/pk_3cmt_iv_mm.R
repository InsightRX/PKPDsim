#' ODE system for PK - 2 compartment IV Michaelis-Menten (non-linear)
#'
#' @param t time vector
#' @param A initial state vector
#' @param p parameters (list)
#'
#' @export
pk_3cmt_iv_mm <- function (t, A, p) {
  with(p, {

    ## Parameter translation & calculations:
    ## required params: VMAX, KM, Q2, V, Q3, V2, V3
    K12 <- Q2/V
    K21 <- Q2/V2
    K12 <- Q2/V
    K21 <- Q2/V2
    K13 <- Q3/V
    K31 <- Q3/V3
    CONC <- A[1]/V

    ## ODE definition:
    return(list(c(
      -VMAX * CONC / (CONC + KM) - K12*A[1] + K21*A[2] - K13*A[1] + K31*A[3] + rate,
      +K12*A[1] - K21*A[2],
      +K13*A[1] - K31*A[3]
    )))

  })
}
## Indicate observation compartment and scaling:
attributes(pk_3cmt_iv_mm) <- list(obs = list (cmt = 1, scale = "V"))
