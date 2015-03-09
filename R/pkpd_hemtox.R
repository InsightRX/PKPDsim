#' ODE system for PKPD - 2 compartment IV, neutropenia model (Friberg et al. JCO 2002)
#'
#' @export
hemtox <- function (t, A, p) {  # ODE system
  with(p, {
    ## PK
    KEL <- CL/V
    K12 <- Q2/V
    K21 <- Q2/V2
    K13 <- Q3/V
    K31 <- Q3/V3
    dAdt_1 <- -KEL*A[1] - K12*A[1] + K21*A[2] - K13*A[1] + K31*A[3] + rate
    dAdt_2 <- +K12*A[1] - K21*A[2]
    dAdt_3 <- +K13*A[1] - K31*A[3]
    conc <- A[1]/V
    ## Neutropenia
    e_drug <- Emax * conc/(EC50 + conc)
    Ktr  <- 4/mtt
    dAdt_4 <- Ktr*A[4] * (1-e_drug) * (circ0/A[8])^gam - Ktr*A[4]
    dAdt_5 <- Ktr*A[4] - Ktr*A[5]
    dAdt_6 <- Ktr*A[5] - Ktr*A[6]
    dAdt_7 <- Ktr*A[6] - Ktr*A[7]
    dAdt_7 <- Ktr*A[6] - Ktr*A[7]
    dAdt_8 <- Ktr*A[7] - Ktr*A[8]
    return ( list ( c (  dAdt_1, dAdt_2, dAdt_3, dAdt_4, dAdt_5, dAdt_6, dAdt_7, dAdt_8 ) ) )
  })
}
