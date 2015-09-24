#' ODE system for PKPD - 2 compartment IV, neutropenia model (Friberg et al. JCO 2002)
#'
#' @param t time vector
#' @param A initial state vector
#' @param p parameters (list)
#'
#' @export
pkpd_hemtox <- function (t, A, p) {  # ODE system
  with(p, {

    ## Parameter translation & calculations:
    KEL <- CL/V
    K12 <- Q2/V
    K21 <- Q2/V2
    K13 <- Q3/V
    K31 <- Q3/V3
    conc <- A[1]/V
    e_drug <- Emax * conc/(EC50 + conc)
    Ktr  <- 4/mtt

    ## ODE definition
    return(list(c(
       -KEL*A[1] - K12*A[1] + K21*A[2] - K13*A[1] + K31*A[3] + rate,
       +K12*A[1] - K21*A[2],
       +K13*A[1] - K31*A[3],
       +Ktr*A[4] * (1-e_drug) * (circ0/A[8])^gam - Ktr*A[4],
       +Ktr*A[4] - Ktr*A[5],
       +Ktr*A[5] - Ktr*A[6],
       +Ktr*A[6] - Ktr*A[7],
       +Ktr*A[7] - Ktr*A[8]
    )))
  })
}
## Indicate observation compartment and scaling:
attributes(pkpd_hemtox) <- list(obs = list (cmt = 8, scale = 1))
