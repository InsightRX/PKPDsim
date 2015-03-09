## Simulation of Docetaxel PK & HemTox model (Friberg et al. JCO 2002)
## Author: Ron Keizer

hemtox <- function (t, A, p) {  # ODE system
  KEL <- p$CL/p$V
  K12 <- p$Q2/p$V
  K21 <- p$Q2/p$V2
  K13 <- p$Q3/p$V
  K31 <- p$Q3/p$V3
  dAdt_1 <- -KEL*A[1] - K12*A[1] + K21*A[2] - K13*A[1] + K31*A[3] + p$rate
  dAdt_2 <- +K12*A[1] - K21*A[2]
  dAdt_3 <- +K13*A[1] - K31*A[3]
  conc <- A[1]/p$V
  e_drug <- p$Emax * conc/(p$EC50 + conc)
  Ktr  <- 4/p$mtt
  dAdt_4 <- Ktr*A[4] * (1-e_drug) * (p$circ0/A[8])^p$gam - Ktr*A[4]
  dAdt_5 <- Ktr*A[4] - Ktr*A[5]
  dAdt_6 <- Ktr*A[5] - Ktr*A[6]
  dAdt_7 <- Ktr*A[6] - Ktr*A[7]
  dAdt_7 <- Ktr*A[6] - Ktr*A[7]
  dAdt_8 <- Ktr*A[7] - Ktr*A[8]
  return ( list ( c (  dAdt_1, dAdt_2, dAdt_3, dAdt_4, dAdt_5, dAdt_6, dAdt_7, dAdt_8 ) ) )
}

