#' ODE system for PK - 2 compartment IV Michaelis-Menten (non-linear)
#'
#' @param t time vector
#' @param A initial state vector
#' @param p parameters (list)
#'
#' @export
pk_2cmt_iv_mm <- function (t, A, p) {
  with(p, {

    ## Parameter translation & calculations:
    ## required params: VMAX, KM, Q, V, V2
    K12 <- Q2/V
    K21 <- Q2/V2
    CONC <- A[1]/V

    ## ODE definition:
    return(list(c(
      -VMAX * CONC / (CONC + KM) - K12*A[1] + K21*A[2] + rate,
      +K12*A[1] - K21*A[2]
    )))

  })
}
## Indicate observation compartment and scaling:
attributes(pk_2cmt_iv_mm) <- list(obs = list (cmt = 1, scale = "V"))
