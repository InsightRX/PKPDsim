#' ODE system for PK - 2 compartment oral with Michaelis-Menten elimination
#'
#' @param t time vector
#' @param A initial state vector
#' @param p parameters (list)
#'
#' @export
pk_2cmt_oral_mm <- function (t, A, p) {
  with(p, {

    ## Parameters required: CL, V, Q, V2, V3, VMAX, KM, KA
    CONC <- A[2]/V
    KEL <- CL/V
    K23 <- Q/V2
    K32 <- Q/V3

    ## ODE definition:
    return(list(c(
      -KA * A[1],
      -VMAX*CONC/(CONC + KM) + KA*A[1] - K23*A[2] + K32*A[3],
       K23*A[2] - K32*A[3]
    )))

  })
}

## Indicate observation compartment and scaling:
attributes(pk_2cmt_oral_mm) <- list(obs = list (cmt = 2, scale = "V"))
