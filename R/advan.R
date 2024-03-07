#' ADVAN-style equations
#'
#' Adapted from Abuhelwa et al. JPET 2015
#'
#' Functions for calculating drug amount in each compartments of the
#' common pharmacokinetic models (1,2,3 compartment IV bolus, IV infusion,
#' and first-order absorption models)
#'
#'  Definitions:
#' - A*last: is the initial amount at the beginning of each time interval (t, t=t2-t1)
#'   of a corresponding compartment (i.e. drug amount at the end of the last time interval)
#' - E* : the sum of Exit (elimination) rate constant of the corresponding compartment.


#' IV bolus- 1 compartment
#' @param d data, a NONMEM style data frame for 1 subject with columns for TIME,
#'   AMT, MDV, DV, CL, V
#' @return Returns a dataframe with populated columns for A1, and DV
#' @references Abuhelwa, A. Y., Foster, D. J. R., Upton, R. N. (2015)
#'   ADVAN-style analytical solutions for common pharmacokinetic models. J
#'   Pharmacol Toxicol Methods 73:42-8. DOI: 10.1016/j.vascn.2015.03.004
#' @keywords internal
OneCompIVbolus <- function(d){

  # set initial values in the compartments
  d$A1[d$TIME==0] <- d$AMT[d$TIME==0]  # drug amount in the central compartment at time zero.

  # This loop advances the solution from one time interval to the next.
  # It calculates micro-rate constants based on individual's PK parameter values and use them to calculate drug amounts in each compartment.
  # It also calculates the concentration in the central compartment.
  for(i in 2:nrow(d)) {
    k10 <- d$CL[i]/d$V[i]
    t <- d$TIME[i]-d$TIME[i-1]
    A1last <- d$A1[i-1]

    d$A1[i] = d$AMT[i]+ A1last*exp(-t*k10)   # Amount in the central compartment
    d$DV[i] = d$A1[i]/d$V[i]                 # Concentration in the central compartment

    d$AUC[i] = d$AUC[i-1] + (d$A1[i-1] - A1last*exp(-t*k10))/d$CL[i]
  }
  d
}

#' IV bolus- 2 compartment
#' @param d data, accepts a NONMEM style data frame for 1 subject with columns for TIME, AMT,MDV, DV, CL, V1, Q, V2
#' @return Returns a dataframe with populated columns for A1, A2, and DV
#' @keywords internal
TwoCompIVbolus <- function(d) {

  # set initial values in the compartments
  d$A1[d$TIME==0] <- d$AMT[d$TIME==0]  # drug amount in the central compartment at time zero.
  d$A2[d$TIME==0] <- 0  # drug amount in the peripheral compartment at time zero.

  # This loop calculates micro-rate constants based on individual's PK parameter values.
  # It uses these values to calculate macro-rate constants (Lambda1/lambda2).
  # Rate constants(micro- and macro) are used in the equations to calculate drug amounts in each compartment.
  # The loop advances the solution from one time interval to the next.
  # It also calculates the concentration in the central compartment.
  for(i in 2:nrow(d)) {
    k10 <- d$CL[i]/d$V[i]
    k12 <- d$Q[i]/d$V[i]
    k21 <- d$Q[i]/d$V2[i]
    k20 <- 0
    E1 <- k10+k12
    E2 <- k21+k20

    # calculate hybrid rate constants
    lambda1 <- 0.5*(k12+k21+k10+sqrt((k12+k21+k10)^2-4*k21*k10))
    lambda2 <- 0.5*(k12+k21+k10-sqrt((k12+k21+k10)^2-4*k21*k10))

    t <- d$TIME[i]-d$TIME[i-1]
    A1last <- d$A1[i-1]
    A2last <- d$A2[i-1]

    A1term = (((A1last*E2+A2last*k21)-A1last*lambda1)*exp(-t*lambda1)-((A1last*E2+A2last*k21)-A1last*lambda2)*exp(-t*lambda2))/(lambda2-lambda1)
    d$A1[i] = A1term + d$AMT[i] #Amount in the central compartment

    A2term = (((A2last*E1+A1last*k12)-A2last*lambda1)*exp(-t*lambda1)-((A2last*E1+A1last*k12)-A2last*lambda2)*exp(-t*lambda2))/(lambda2-lambda1)
    d$A2[i] = A2term            #Amount in the peripheral compartment

    d$DV[i] <- d$A1[i]/d$V[i]  #Concentration in the central compartment

    d$AUC[i] = d$AUC[i-1] + ((d$A1[i-1]-A1term) + (d$A2[i-1] - A2term))/d$CL[i]

  }
  d
}

#' IV bolus- 3 compartment
#' @param d data, Accepts a NONMEM style data frame for 1 subject with columns for TIME, AMT,MDV,DV, CL, V1, Q12, V2, Q13, V3
#' @return Returns a dataframe with populated columns for A1, A2, A3,and DV
#' @keywords internal
ThreeCompIVbolus <- function(d) {

  # set initial values in the compartments
  d$A1[d$TIME==0] <- d$AMT[d$TIME==0]  # drug amount in the central compartment at time zero.
  d$A2[d$TIME==0] <- 0   # drug amount in the 1st-peripheral compartment at time zero.
  d$A3[d$TIME==0] <- 0   # drug amount in the 2nd-peripheral compartment at time zero.

  # This loop calculates micro-rate constants based on individual's PK parameter values.
  # It uses these values to calculate macro-rate constants (Lambda1/lambda2/lambda3).
  # Rate constants(micro- and macro) are used in the equations to calculate drug amounts in each compartment.
  # The loop advances the solution from one time interval to the next.
  # It also calculates the concentration in the central compartment.
  for(i in 2:nrow(d))
  {
    k10 <- d$CL[i]/d$V[i]
    k12 <- d$Q[i]/d$V[i]
    k21 <- k12*d$V[i]/d$V2[i]
    k13 <- d$Q2[i]/d$V[i]
    k31 <- k13*d$V[i]/d$V3[i]
    k20 <- 0
    k30 <- 0
    E1 <- k10+k12+k13
    E2 <- k21+k20
    E3 <- k31+k30

    #calculate hybrid rate constants
    a <- E1+E2+E3
    b <- E1*E2+E3*(E1+E2)-k12*k21-k13*k31
    c <- E1*E2*E3-E3*k12*k21-E2*k13*k31

    m <- (3*b - a^2)/3
    n <- (2*a^3 - 9*a*b + 27*c)/27
    Q <- (n^2)/4 + (m^3)/27

    alpha <- sqrt(-1*Q)
    beta <- -1*n/2
    gamma <- sqrt(beta^2+alpha^2)
    theta <- atan2(alpha,beta)

    lambda1 <- a/3 + gamma^(1/3)*(cos(theta/3) + sqrt(3)*sin(theta/3))
    lambda2 <- a/3 + gamma^(1/3)*(cos(theta/3) - sqrt(3)*sin(theta/3))
    lambda3 <- a/3 -(2*gamma^(1/3)*cos(theta/3))

    t <- d$TIME[i]-d$TIME[i-1]
    A1last <- d$A1[i-1]
    A2last <- d$A2[i-1]
    A3last <- d$A3[i-1]

    B = A2last*k21+A3last*k31
    C = E3*A2last*k21+E2*A3last*k31
    I = A1last*k12*E3-A2last*k13*k31+A3last*k12*k31
    J = A1last*k13*E2+A2last*k13*k21-A3last*k12*k21

    A1term1 = A1last*(exp(-t*lambda1)*(E2-lambda1)*(E3-lambda1)/((lambda2-lambda1)*(lambda3-lambda1))+exp(-t*lambda2)*(E2-lambda2)*(E3-lambda2)/((lambda1-lambda2)*(lambda3-lambda2))+exp(-t*lambda3)*(E2-lambda3)*(E3-lambda3)/((lambda1-lambda3)*(lambda2-lambda3)))
    A1term2 = exp(-t*lambda1)*(C-B*lambda1)/((lambda1-lambda2)*(lambda1-lambda3))+exp(-t*lambda2)*(B*lambda2-C)/((lambda1-lambda2)*(lambda2-lambda3))+exp(-t*lambda3)*(B*lambda3-C)/((lambda1-lambda3)*(lambda3-lambda2))

    d$A1[i] <- d$AMT[i]+(A1term1+A1term2)  #Amount in the central compartment

    A2term1 = A2last*(exp(-t*lambda1)*(E1-lambda1)*(E3-lambda1)/((lambda2-lambda1)*(lambda3-lambda1))+exp(-t*lambda2)*(E1-lambda2)*(E3-lambda2)/((lambda1-lambda2)*(lambda3-lambda2))+exp(-t*lambda3)*(E1-lambda3)*(E3-lambda3)/((lambda1-lambda3)*(lambda2-lambda3)))
    A2term2 = exp(-t*lambda1)*(I-A1last*k12*lambda1)/((lambda1-lambda2)*(lambda1-lambda3))+exp(-t*lambda2)*(A1last*k12*lambda2-I)/((lambda1-lambda2)*(lambda2-lambda3))+exp(-t*lambda3)*(A1last*k12*lambda3-I)/((lambda1-lambda3)*(lambda3-lambda2))

    d$A2[i] <- A2term1+A2term2             #Amount in the first-peripheral compartment

    A3term1 = A3last*(exp(-t*lambda1)*(E1-lambda1)*(E2-lambda1)/((lambda2-lambda1)*(lambda3-lambda1))+exp(-t*lambda2)*(E1-lambda2)*(E2-lambda2)/((lambda1-lambda2)*(lambda3-lambda2))+exp(-t*lambda3)*(E1-lambda3)*(E2-lambda3)/((lambda1-lambda3)*(lambda2-lambda3)))
    A3term2 = exp(-t*lambda1)*(J-A1last*k13*lambda1)/((lambda1-lambda2)*(lambda1-lambda3))+exp(-t*lambda2)*(A1last*k13*lambda2-J)/((lambda1-lambda2)*(lambda2-lambda3))+exp(-t*lambda3)*(A1last*k13*lambda3-J)/((lambda1-lambda3)*(lambda3-lambda2))

    d$A3[i] <- A3term1+A3term2            #Amount in the second-peripheral compartment

    d$AUC[i] = d$AUC[i-1] + (d$A1[i-1]-(A1term1+A1term2) + d$A2[i-1]-(A2term1+A2term2) + d$A3[i-1]-(A3term1+A3term2))/d$CL[i]

    d$DV[i] <- d$A1[i]/d$V[i]            #Concentration in the central compartment
  }
  d
}

#' IV infusion- 1 compartment
#' @param d data, accepts a NONMEM style data frame for 1 subject with columns for TIME, AMT,MDV, RATE, RATEALL, DV, CL, V
#' @return Returns a dataframe with populated columns for A1, and DV
#' @keywords internal
OneCompIVinfusion <- function(d) {

  #set initial values in the compartments
  d$A1[d$TIME==0] <- 0  # drug amount in the central compartment at time zero.

  # This loop calculates micro-rate constants based on individual's PK parameter values.
  # Rate constants are used in the equations to calculate drug amounts in a compartment.
  # The loop advances the solution from one time interval to the next.
  # It also calculates the concentration in the central compartment.
  for(i in 2:nrow(d)) {
    k10 <- d$CL[i]/d$V[i]

    t <- d$TIME[i]-d$TIME[i-1]
    A1last <- d$A1[i-1]
    Doserate <- d$RATEALL[i]

    d$A1[i] = Doserate/k10*(1-exp(-t*k10))+A1last*exp(-t*k10)       #Amount in the central compartment

    d$DV[i] <- d$A1[i]/d$V[i]                                       #Concentration in the central compartment

    if(Doserate > 0) {
      # AUC during infusion is total AUC of dose (A/CL) minus the AUC still to be eliminated (Amount from dose at EOI/CL)
      d$AUC[i] <- d$AUC[i-1] + (Doserate*t)/d$CL[i] - (d$A1[i]-A1last)/d$CL[i]
    } else {
      d$AUC[i] = d$AUC[i-1] + (d$A1[i-1] - d$A1[i])/d$CL[i]
    }

  }
  d
}

#' IV infusion- 2 compartment
#' @param d data, accepts a NONMEM style data frame for 1 subject with columns for TIME, AMT,MDV, RATE, RATEALL, DV, CL, V1, Q, V2
#' @return Returns a dataframe with populated columns for A1, A2, and DV
#' @keywords internal
TwoCompIVinfusion <- function(d) {

  # set initial values in the compartments
  d$A1[d$TIME==0] <- 0  # drug amount in the central compartment at time zero.
  d$A2[d$TIME==0] <- 0  # drug amount in the peripheral compartment at time zero.

  # This loop calculates micro-rate constants based on individual's PK parameter values.
  # It uses these values to calculate macro-rate constants (Lambda1/lambda2).
  # Rate constants(micro- and macro) are used in the equations to calculate drug amounts in each compartment.
  # The loop advances the solution from one time interval to the next.
  # It also calculates the concentration in the central compartment.
  for(i in 2:nrow(d)) {
    k10 <- d$CL[i]/d$V[i]
    k12 <- d$Q[i]/d$V[i]
    k21 <- d$Q[i]/d$V2[i]
    k20 <- 0
    E1 <- k10+k12
    E2 <- k21+k20

    #calculate hybrid rate constants
    lambda1 = 0.5*((E1+E2)+sqrt((E1+E2)^2-4*(E1*E2-k12*k21)))
    lambda2 = 0.5*((E1+E2)-sqrt((E1+E2)^2-4*(E1*E2-k12*k21)))

    t <- d$TIME[i]-d$TIME[i-1]
    A1last <- d$A1[i-1]
    A2last <- d$A2[i-1]
    Doserate <- d$RATEALL[i]

    A1term1 = (((A1last*E2+Doserate+A2last*k21)-A1last*lambda1)*exp(-t*lambda1)-((A1last*E2+Doserate+A2last*k21)-A1last*lambda2)*exp(-t*lambda2))/(lambda2-lambda1)
    A1term2 = Doserate*E2*(1/(lambda1*lambda2)+exp(-t*lambda1)/(lambda1*(lambda1-lambda2))-exp(-t*lambda2)/(lambda2*(lambda1-lambda2)))

    d$A1[i] <- A1term1+A1term2    #Amount in the central compartment

    A2term1 = (((A2last*E1+A1last*k12)-A2last*lambda1)*exp(-t*lambda1)-((A2last*E1+A1last*k12)-A2last*lambda2)*exp(-t*lambda2))/(lambda2-lambda1)
    A2term2 = Doserate*k12*(1/(lambda1*lambda2)+exp(-t*lambda1)/(lambda1*(lambda1-lambda2))-exp(-t*lambda2)/(lambda2*(lambda1-lambda2)))

    d$A2[i] <- A2term1+A2term2   #Amount in the peripheral compartment

    d$DV[i] <- d$A1[i]/d$V[i]   #Concentration in the central compartment

    if(Doserate > 0) {
      # AUC during infusion is total AUC of dose (A/CL) minus the AUC still to be eliminated (Amount from dose at EOI/CL)
      d$AUC[i] <- d$AUC[i-1] + (Doserate*t)/d$CL[i] - (d$A1[i]-A1last + d$A2[i] - A2last)/d$CL[i]
    } else {
      d$AUC[i] = d$AUC[i-1] + (d$A1[i-1] + d$A2[i-1] - d$A1[i] - d$A2[i])/d$CL[i]
    }

  }
  d
}

#' IV infusion- 3 compartment
#' @param d data, Accepts a NONMEM style data frame for 1 subject with columns for TIME, AMT,MDV,RATE, RATEALL, DV, CL, V1, Q12, V2, Q13, V3
#' @return Returns a dataframe with populated columns for A1, A2, A3,and DV
#' @keywords internal
ThreeCompIVinfusion <- function(d) {

  # set initial values in the compartments
  d$A1[d$TIME==0] <- 0   # drug amount in the central compartment at time zero.
  d$A2[d$TIME==0] <- 0   # drug amount in the 1st-peripheral compartment at time zero.
  d$A3[d$TIME==0] <- 0   # drug amount in the 2nd-peripheral compartment at time zero.

  # This loop calculates micro-rate constants based on individual's PK parameter values.
  # It uses these values to calculate macro-rate constants (Lambda1/lambda2/lambda3).
  # Rate constants(micro- and macro) are used in the equations to calculate drug amounts in each compartment.
  # The loop advances the solution from one time interval to the next.
  # It also calculates the concentration in the central compartment.
  for(i in 2:nrow(d)) {
    k10 <- d$CL[i]/d$V[i]
    k12 <- d$Q[i]/d$V[i]
    k21 <- k12*d$V[i]/d$V2[i]
    k13 <- d$Q2[i]/d$V[i]
    k31 <- k13*d$V[i]/d$V3[i]
    k20 <- 0
    k30 <- 0
    E1 <- k10+k12+k13
    E2 <- k21+k20
    E3 <- k31+k30

    #calculate hybrid rate constants
    a <- E1+E2+E3
    b <- E1*E2+E3*(E1+E2)-k12*k21-k13*k31
    c <- E1*E2*E3-E3*k12*k21-E2*k13*k31

    m <- (3*b - a^2)/3
    n <- (2*a^3 - 9*a*b + 27*c)/27
    Q <- (n^2)/4 + (m^3)/27

    alpha <- sqrt(-1*Q)
    beta <- -1*n/2
    gamma <- sqrt(beta^2+alpha^2)
    theta <- atan2(alpha,beta)

    lambda1 <- a/3 + gamma^(1/3)*(cos(theta/3) + sqrt(3)*sin(theta/3))
    lambda2 <- a/3 + gamma^(1/3)*(cos(theta/3) - sqrt(3)*sin(theta/3))
    lambda3 <- a/3 -(2*gamma^(1/3)*cos(theta/3))

    t <- d$TIME[i]-d$TIME[i-1]
    A1last <- d$A1[i-1]
    A2last <- d$A2[i-1]
    A3last <- d$A3[i-1]
    Doserate <- d$RATEALL[i]

    B = A2last*k21+A3last*k31
    C = E3*A2last*k21+E2*A3last*k31
    I = A1last*k12*E3-A2last*k13*k31+A3last*k12*k31
    J = A1last*k13*E2+A2last*k13*k21-A3last*k12*k21

    A1term1 = A1last*(exp(-t*lambda1)*(E2-lambda1)*(E3-lambda1)/((lambda2-lambda1)*(lambda3-lambda1))+exp(-t*lambda2)*(E2-lambda2)*(E3-lambda2)/((lambda1-lambda2)*(lambda3-lambda2))+exp(-t*lambda3)*(E2-lambda3)*(E3-lambda3)/((lambda1-lambda3)*(lambda2-lambda3)))
    A1term2 = exp(-t*lambda1)*(C-B*lambda1)/((lambda1-lambda2)*(lambda1-lambda3))+exp(-t*lambda2)*(B*lambda2-C)/((lambda1-lambda2)*(lambda2-lambda3))+exp(-t*lambda3)*(B*lambda3-C)/((lambda1-lambda3)*(lambda3-lambda2))
    A1term3 = Doserate*((E2*E3)/(lambda1*lambda2*lambda3)-exp(-t*lambda1)*(E2-lambda1)*(E3-lambda1)/(lambda1*(lambda2-lambda1)*(lambda3-lambda1))-exp(-t*lambda2)*(E2-lambda2)*(E3-lambda2)/(lambda2*(lambda1-lambda2)*(lambda3-lambda2))-exp(-t*lambda3)*(E2-lambda3)*(E3-lambda3)/(lambda3*(lambda1-lambda3)*(lambda2-lambda3)))

    d$A1[i] <- A1term1+A1term2+A1term3    #Amount in the central compartment

    A2term1 = A2last*(exp(-t*lambda1)*(E1-lambda1)*(E3-lambda1)/((lambda2-lambda1)*(lambda3-lambda1))+exp(-t*lambda2)*(E1-lambda2)*(E3-lambda2)/((lambda1-lambda2)*(lambda3-lambda2))+exp(-t*lambda3)*(E1-lambda3)*(E3-lambda3)/((lambda1-lambda3)*(lambda2-lambda3)))
    A2term2 = exp(-t*lambda1)*(I-A1last*k12*lambda1)/((lambda1-lambda2)*(lambda1-lambda3))+exp(-t*lambda2)*(A1last*k12*lambda2-I)/((lambda1-lambda2)*(lambda2-lambda3))+exp(-t*lambda3)*(A1last*k12*lambda3-I)/((lambda1-lambda3)*(lambda3-lambda2))
    A2term3 = Doserate*k12*(E3/(lambda1*lambda2*lambda3)-exp(-t*lambda1)*(E3-lambda1)/(lambda1*(lambda2-lambda1)*(lambda3-lambda1))-exp(-t*lambda2)*(E3-lambda2)/(lambda2*(lambda1-lambda2)*(lambda3-lambda2))-exp(-t*lambda3)*(E3-lambda3)/(lambda3*(lambda1-lambda3)*(lambda2-lambda3)))

    d$A2[i] <- A2term1+A2term2+A2term3    #Amount in the first-peripheral compartment

    A3term1 = A3last*(exp(-t*lambda1)*(E1-lambda1)*(E2-lambda1)/((lambda2-lambda1)*(lambda3-lambda1))+exp(-t*lambda2)*(E1-lambda2)*(E2-lambda2)/((lambda1-lambda2)*(lambda3-lambda2))+exp(-t*lambda3)*(E1-lambda3)*(E2-lambda3)/((lambda1-lambda3)*(lambda2-lambda3)))
    A3term2 = exp(-t*lambda1)*(J-A1last*k13*lambda1)/((lambda1-lambda2)*(lambda1-lambda3))+exp(-t*lambda2)*(A1last*k13*lambda2-J)/((lambda1-lambda2)*(lambda2-lambda3))+exp(-t*lambda3)*(A1last*k13*lambda3-J)/((lambda1-lambda3)*(lambda3-lambda2))
    A3term3 = Doserate*k13*(E2/(lambda1*lambda2*lambda3)-exp(-t*lambda1)*(E2-lambda1)/(lambda1*(lambda2-lambda1)*(lambda3-lambda1))-exp(-t*lambda2)*(E2-lambda2)/(lambda2*(lambda1-lambda2)*(lambda3-lambda2))-exp(-t*lambda3)*(E2-lambda3)/(lambda3*(lambda1-lambda3)*(lambda2-lambda3)))

    d$A3[i] <- A3term1+A3term2+A3term3  #Amount in the second-peripheral compartment

    d$DV[i] <- d$A1[i]/d$V[i]          #Concentration in the central compartment

    if(Doserate > 0) {
      # AUC during infusion is total AUC of dose (A/CL) minus the AUC still to be eliminated (Amount from dose at EOI/CL)
      d$AUC[i] <- d$AUC[i-1] + (Doserate*t)/d$CL[i] - (d$A1[i]-A1last + d$A2[i]-A2last + d$A3[i]-A3last)/d$CL[i]
    } else {
      d$AUC[i] = d$AUC[i-1] + (d$A1[i-1] - d$A1[i] + d$A2[i-1] - d$A2[i] + d$A3[i-1] - d$A3[i])/d$CL[i]
    }

  }
  d
}

#' 3-compartment IV infusion with first-order metabolite formation
#' @param d data, accepts a NONMEM style data frame for 1 subject with columns for TIME, AMT,MDV,RATE, RATEALL, DV, CL, V1, Q12, V2, Q13, V3, CLM,VM,km
#' @return Returns a dataframe with populated columns for A1, A2, A3,and DV
#' @keywords internal
ThreeCompIVinfusionMetab <- function(d) {

  # set initial values in the compartments
  d$A1[d$TIME==0] <- 0   # drug amount in the central compartment at time zero.
  d$A2[d$TIME==0] <- 0   # drug amount in the 1st-peripheral compartment at time zero.
  d$A3[d$TIME==0] <- 0   # drug amount in the 2nd-peripheral compartment at time zero.
  d$Am[d$TIME==0] <- 0   # drug amount in the metabolite compartment at time zero.

  # This loop calculates micro-rate constants based on individual's PK parameter values.
  # It uses these values to calculate macro-rate constants (Lambda1/lambda2/lambda3).
  # Rate constants(micro- and macro) are used in the equations to calculate drug amounts in each compartment.
  # The loop advances the solution from one time interval to the next.
  # It also calculates the concentration in the central compartment.
  for(i in 2:nrow(d)) {
    k10 <- d$CL[i]/d$V[i]
    k12 <- d$Q12[i]/d$V[i]
    k21 <- k12*d$V[i]/d$V2[i]
    k13 <- d$Q13[i]/d$V[i]
    k31 <- k13*d$V[i]/d$V3[i]
    km  <- d$km[i]
    kme <- d$CLm[i]/d$Vm[i]
    k20 <- 0
    k30 <- 0
    E1 <- k10+k12+k13+km
    E2 <- k21+k20
    E3 <- k31+k30

    #calculate hybrid rate constants
    a <- E1+E2+E3
    b <- E1*E2+E3*(E1+E2)-k12*k21-k13*k31
    c <- E1*E2*E3-E3*k12*k21-E2*k13*k31

    m <- (3*b - a^2)/3
    n <- (2*a^3 - 9*a*b + 27*c)/27
    Q <- (n^2)/4 + (m^3)/27

    alpha <- sqrt(-1*Q)
    beta <- -1*n/2
    gamma <- sqrt(beta^2+alpha^2)
    theta <- atan2(alpha,beta)

    lambda1 <- a/3 + gamma^(1/3)*(cos(theta/3) + sqrt(3)*sin(theta/3))
    lambda2 <- a/3 + gamma^(1/3)*(cos(theta/3) - sqrt(3)*sin(theta/3))
    lambda3 <- a/3 -(2*gamma^(1/3)*cos(theta/3))

    t <- d$TIME[i]-d$TIME[i-1]
    A1last <- d$A1[i-1]
    A2last <- d$A2[i-1]
    A3last <- d$A3[i-1]
	  Amlast <- d$Am[i-1]
    Doserate <- d$RATEALL[i]

    B = A2last*k21+A3last*k31
    C = E3*A2last*k21+E2*A3last*k31
    I = A1last*k12*E3-A2last*k13*k31+A3last*k12*k31
    J = A1last*k13*E2+A2last*k13*k21-A3last*k12*k21

    A1term1 = A1last*(exp(-t*lambda1)*(E2-lambda1)*(E3-lambda1)/((lambda2-lambda1)*(lambda3-lambda1))+exp(-t*lambda2)*(E2-lambda2)*(E3-lambda2)/((lambda1-lambda2)*(lambda3-lambda2))+exp(-t*lambda3)*(E2-lambda3)*(E3-lambda3)/((lambda1-lambda3)*(lambda2-lambda3)))
    A1term2 = exp(-t*lambda1)*(C-B*lambda1)/((lambda1-lambda2)*(lambda1-lambda3))+exp(-t*lambda2)*(B*lambda2-C)/((lambda1-lambda2)*(lambda2-lambda3))+exp(-t*lambda3)*(B*lambda3-C)/((lambda1-lambda3)*(lambda3-lambda2))
    A1term3 = Doserate*((E2*E3)/(lambda1*lambda2*lambda3)-exp(-t*lambda1)*(E2-lambda1)*(E3-lambda1)/(lambda1*(lambda2-lambda1)*(lambda3-lambda1))-exp(-t*lambda2)*(E2-lambda2)*(E3-lambda2)/(lambda2*(lambda1-lambda2)*(lambda3-lambda2))-exp(-t*lambda3)*(E2-lambda3)*(E3-lambda3)/(lambda3*(lambda1-lambda3)*(lambda2-lambda3)))

    d$A1[i] <- A1term1+A1term2+A1term3   #Amount in the central compartment

    A2term1 = A2last*(exp(-t*lambda1)*(E1-lambda1)*(E3-lambda1)/((lambda2-lambda1)*(lambda3-lambda1))+exp(-t*lambda2)*(E1-lambda2)*(E3-lambda2)/((lambda1-lambda2)*(lambda3-lambda2))+exp(-t*lambda3)*(E1-lambda3)*(E3-lambda3)/((lambda1-lambda3)*(lambda2-lambda3)))
    A2term2 = exp(-t*lambda1)*(I-A1last*k12*lambda1)/((lambda1-lambda2)*(lambda1-lambda3))+exp(-t*lambda2)*(A1last*k12*lambda2-I)/((lambda1-lambda2)*(lambda2-lambda3))+exp(-t*lambda3)*(A1last*k12*lambda3-I)/((lambda1-lambda3)*(lambda3-lambda2))
    A2term3 = Doserate*k12*(E3/(lambda1*lambda2*lambda3)-exp(-t*lambda1)*(E3-lambda1)/(lambda1*(lambda2-lambda1)*(lambda3-lambda1))-exp(-t*lambda2)*(E3-lambda2)/(lambda2*(lambda1-lambda2)*(lambda3-lambda2))-exp(-t*lambda3)*(E3-lambda3)/(lambda3*(lambda1-lambda3)*(lambda2-lambda3)))

    d$A2[i] <- A2term1+A2term2+A2term3   #Amount in the first-peripheral compartment

    A3term1 = A3last*(exp(-t*lambda1)*(E1-lambda1)*(E2-lambda1)/((lambda2-lambda1)*(lambda3-lambda1))+exp(-t*lambda2)*(E1-lambda2)*(E2-lambda2)/((lambda1-lambda2)*(lambda3-lambda2))+exp(-t*lambda3)*(E1-lambda3)*(E2-lambda3)/((lambda1-lambda3)*(lambda2-lambda3)))
    A3term2 = exp(-t*lambda1)*(J-A1last*k13*lambda1)/((lambda1-lambda2)*(lambda1-lambda3))+exp(-t*lambda2)*(A1last*k13*lambda2-J)/((lambda1-lambda2)*(lambda2-lambda3))+exp(-t*lambda3)*(A1last*k13*lambda3-J)/((lambda1-lambda3)*(lambda3-lambda2))
    A3term3 = Doserate*k13*(E2/(lambda1*lambda2*lambda3)-exp(-t*lambda1)*(E2-lambda1)/(lambda1*(lambda2-lambda1)*(lambda3-lambda1))-exp(-t*lambda2)*(E2-lambda2)/(lambda2*(lambda1-lambda2)*(lambda3-lambda2))-exp(-t*lambda3)*(E2-lambda3)/(lambda3*(lambda1-lambda3)*(lambda2-lambda3)))

    d$A3[i] <- A3term1+A3term2+A3term3  #Amount in the second peripheral compartment

    Amterm1 = Amlast*exp(-t*kme) +km*A1last*(exp(-t*lambda1)*(E2-lambda1)*(E3-lambda1)/((lambda2-lambda1)*(lambda3-lambda1)*(kme-lambda1))+exp(-t*lambda2)*(E2-lambda2)*(E3-lambda2)/((kme-lambda2)*(lambda1-lambda2)*(lambda3-lambda2))+exp(-t*lambda3)*(E2-lambda3)*(E3-lambda3)/((kme-lambda3)*(lambda1-lambda3)*(lambda2-lambda3))+exp(-t*kme)*(E2-kme)*(E3-kme)/((lambda1-kme)*(lambda2-kme)*(lambda3-kme)))
    Amterm2 = km*(exp(-t*lambda1)*(B*lambda1-C)/((lambda1-lambda2)*(lambda1-lambda3)*(lambda1-kme))+exp(-t*lambda2)*(C-B*lambda2)/((lambda1-lambda2)*(lambda2-lambda3)*(lambda2-kme))+exp(-t*lambda3)*(C-B*lambda3)/((lambda1-lambda3)*(lambda3-lambda2)*(lambda3-kme))-exp(-t*kme)*(B*kme-C)/((lambda1-kme)*(kme-lambda2)*(kme-lambda3)))
    Amterm3 = km*Doserate*((E2*E3)/(lambda1*lambda2*lambda3*kme)-exp(-t*lambda1)*(E2-lambda1)*(E3-lambda1)/(lambda1*(kme-lambda1)*(lambda2-lambda1)*(lambda3-lambda1))-exp(-t*lambda2)*(E2-lambda2)*(E3-lambda2)/(lambda2*(kme-lambda2)*(lambda1-lambda2)*(lambda3-lambda2))-exp(-t*lambda3)*(E2-lambda3)*(E3-lambda3)/(lambda3*(kme-lambda3)*(lambda1-lambda3)*(lambda2-lambda3))-exp(-t*kme)*(E2-kme)*(E3-kme)/(kme*(lambda1-kme)*(lambda2-kme)*(lambda3-kme)))

    d$Am[i] = Amterm1+Amterm2+Amterm3   #Amount in the metabolite compartment

    d$DV[i] <- d$A1[i]/d$V[i]          #Concentration in the central compartment

    if(Doserate > 0) {
      # AUC during infusion is total AUC of dose (A/CL) minus the AUC still to be eliminated (Amount from dose at EOI/CL)
      d$AUC[i] <- d$AUC[i-1] + (Doserate*t)/d$CL[i] - (d$A1[i]-A1last)/d$CL[i]
    } else {
      d$AUC[i] <- d$AUC[i-1] + ((A1last * (1-exp(-t*k10)))/k10)/d$V[i]  # regular AUC calculation
    }

  }
  d
}

#' first-order absorption 1 compartment
#' @param d data, accepts a NONMEM style data frame for 1 subject with columns for TIME, AMT,MDV,DV, CL, V, KA & F1
#' @return Returns a dataframe with populated columns for A1, A2 and DV
#' @keywords internal
OneCompOral <- function(d) {

  # set initial values in the compartments
  d$A1[d$TIME==0] <- d$AMT[d$TIME==0]*d$F1[1] #drug amount in the absorption compartment at time zero.
  d$A2[d$TIME==0] <- 0                  #drug amount in the central compartment at time zero.

  # This loop calculates micro-rate constants based on individual's PK parameter values.
  # Rate constants are used, along other parameters,to calculate drug amounts in each compartment.
  # The loop advances the solution from one time interval to the next.
  # It also calculates the concentration in the central compartment.

  for(i in 2:nrow(d)) {

    k10  <- d$CL[i]/d$V[i]
    KA   <- d$KA[i]
    t <- d$TIME[i]-d$TIME[i-1]
    A1last <- d$A1[i-1]
    A2last <- d$A2[i-1]

    A2last <- A1last*KA/(KA-k10)*(exp(-t*k10)-exp(-t*KA))+A2last*exp(-t*k10)
    A1last <- A1last*exp(-1*t*KA)

    d$A2[i] <- A2last             #Amount in the central compartment
    d$A1[i] <- A1last + d$AMT[i]*d$F1[i]  #Amount in the absorption compartment

    d$DV[i] <- d$A2[i]/d$V[i]     #Concentration in the central compartment

  }
  d
}

#' First-order absorption- 2 compartment
#' @param d data, accepts a NONMEM style data frame for 1 subject with columns for TIME, AMT,MDV,DV, CL, V2, Q, V3, KA & F1
#' @return Returns a dataframe with populated columns for A1, A2, A3 and DV
#' @keywords internal
TwoCompOral <- function(d) {
  # set initial values in the compartments
  d$A1[d$TIME==0] <- d$AMT[d$TIME==0]*d$F1[1]  # Amount in the absorption compartment at time zero.
  d$A2[d$TIME==0] <- 0                   # Amount in the central compartment at time zero.
  d$A3[d$TIME==0] <- 0                   # Amount in the peripheral compartment at time zero.

  # This loop calculates micro-rate constants based on individual's PK parameter values.
  # It uses these values to calculate macro-rate constants (Lambda1/lambda2).
  # Rate constants(micro- and macro), along other parameters, are used in the equations to calculate drug amounts in each compartment.
  # The loop advances the solution from one time interval to the next.
  # It also calculates the concentration in the central compartment.

  for(i in 2:nrow(d)) {

    k20 <- d$CL[i]/d$V[i]
    k23 <- d$Q[i]/d$V[i]
    k32 <- d$Q[i]/d$V2[i]
  	KA  <- d$KA[i]
	  k30 <- 0
    E2 <- k20+k23
    E3 <- k32+k30

    #calculate hybrid rate constants
    lambda1 = 0.5*((E2+E3)+sqrt((E2+E3)^2-4*(E2*E3-k23*k32)))
    lambda2 = 0.5*((E2+E3)-sqrt((E2+E3)^2-4*(E2*E3-k23*k32)))

    t <- d$TIME[i]-d$TIME[i-1]
    A2last <- d$A2[i-1]
    A3last <- d$A3[i-1]
    A1last <- d$A1[i-1]

    A2term1 = (((A2last*E3+A3last*k32)-A2last*lambda1)*exp(-t*lambda1)-((A2last*E3+A3last*k32)-A2last*lambda2)*exp(-t*lambda2))/(lambda2-lambda1)
    A2term2 = A1last*KA*(exp(-t*KA)*(E3-KA)/((lambda1-KA)*(lambda2-KA))+exp(-t*lambda1)*(E3-lambda1)/((lambda2-lambda1)*(KA-lambda1))+exp(-t*lambda2)*(E3-lambda2)/((lambda1-lambda2)*(KA-lambda2)))
    d$A2[i] = A2term1+A2term2  #Amount in the central compartment

    A3term1 = (((A3last*E2+A2last*k23)-A3last*lambda1)*exp(-t*lambda1)-((A3last*E2+A2last*k23)-A3last*lambda2)*exp(-t*lambda2))/(lambda2-lambda1)
    A3term2 = A1last*KA*k23*(exp(-t*KA)/((lambda1-KA)*(lambda2-KA))+exp(-t*lambda1)/((lambda2-lambda1)*(KA-lambda1))+exp(-t*lambda2)/((lambda1-lambda2)*(KA-lambda2)))
    d$A3[i] = A3term1+A3term2  #Amount in the peripheral compartment

    A1last = A1last*exp(-t*KA)
    d$A1[i] = A1last + d$AMT[i]*d$F1[i]  #Amount in the absorption compartment

    d$DV[i] <- d$A2[i]/d$V[i]    #Concentration in the central compartment

  }
  d
}

#' first-order absorption- 3 compartment
#' @param d data, accepts a NONMEM style data frame for 1 subject with columns for TIME, AMT,MDV,DV, CL, V2, Q3, V3, Q4, V4, KA & F1
#' @return Returns a dataframe with populated columns for A1, A2, A3, A4 and DV
#' @keywords internal
ThreeCompOral <- function(d) {

  # set initial values in the compartments
  d$A1[d$TIME==0] <- d$AMT[d$TIME==0]*d$F1[1]    # Amount in the absorption compartment at time zero.
  d$A2[d$TIME==0] <- 0                   # Amount in the central compartment at time zero.
  d$A3[d$TIME==0] <- 0                   # Amount in the 1st peripheral compartment at time zero.
  d$A4[d$TIME==0] <- 0                   # Amount in the 2nd peripheral compartment at time zero.

  # This loop calculates micro-rate constants based on individual's PK parameter values.
  # It uses these values to calculate macro-rate constants (Lambda1/lambda2/lambda3).
  # Rate constants(micro- and macro), along other parameters, are used in the equations to calculate drug amounts in each compartment.
  # The loop advances the solution from one time interval to the next.
  # It also calculates the concentration in the central compartment.

  for(i in 2:nrow(d))
  {

    k20 <- d$CL[i]/d$V[i]
    k23 <- d$Q[i]/d$V[i]
    k32 <- k23*d$V[i]/d$V2[i]
    k24 <- d$Q2[i]/d$V[i]
    k42 <- k24*d$V[i]/d$V3[i]
	  KA  <- d$KA[i]
    k30 <- 0
    k40 <- 0
    E2 <- k20+k23+k24
    E3 <- k32+k30
    E4 <- k42+k40

    #calculate hybrid rate constants
    a <- E2+E3+E4
    b <- E2*E3+E4*(E2+E3)-k23*k32-k24*k42
    c <- E2*E3*E4-E4*k23*k32-E3*k24*k42

    m <- (3*b - a^2)/3
    n <- (2*a^3 - 9*a*b + 27*c)/27
    Q <- (n^2)/4 + (m^3)/27

    alpha <- sqrt(-1*Q)
    beta <- -1*n/2
    gamma <- sqrt(beta^2+alpha^2)
    theta <- atan2(alpha,beta)

    lambda1 <- a/3 + gamma^(1/3)*(cos(theta/3) + sqrt(3)*sin(theta/3))
    lambda2 <- a/3 + gamma^(1/3)*(cos(theta/3) - sqrt(3)*sin(theta/3))
    lambda3 <- a/3 -(2*gamma^(1/3)*cos(theta/3))

    t <- d$TIME[i]-d$TIME[i-1]
    A1last <- d$A1[i-1]
    A2last <- d$A2[i-1]
    A3last <- d$A3[i-1]
    A4last <- d$A4[i-1]

    B = A3last*k32+A4last*k42
    C = E4*A3last*k32+E3*A4last*k42
    I = A2last*k23*E4-A3last*k24*k42+A4last*k23*k42
    J = A2last*k24*E3+A3last*k24*k32-A4last*k23*k32

    A2term1 = A2last*(exp(-t*lambda1)*(E3-lambda1)*(E4-lambda1)/((lambda2-lambda1)*(lambda3-lambda1))+exp(-t*lambda2)*(E3-lambda2)*(E4-lambda2)/((lambda1-lambda2)*(lambda3-lambda2))+exp(-t*lambda3)*(E3-lambda3)*(E4-lambda3)/((lambda1-lambda3)*(lambda2-lambda3)))
    A2term2 = exp(-t*lambda1)*(C-B*lambda1)/((lambda1-lambda2)*(lambda1-lambda3))+exp(-t*lambda2)*(B*lambda2-C)/((lambda1-lambda2)*(lambda2-lambda3))+exp(-t*lambda3)*(B*lambda3-C)/((lambda1-lambda3)*(lambda3-lambda2))
    A2term3 = A1last*KA*(exp(-t*lambda1)*(E3-lambda1)*(E4-lambda1)/((lambda2-lambda1)*(lambda3-lambda1)*(KA-lambda1))+exp(-t*lambda2)*(E3-lambda2)*(E4-lambda2)/((lambda1-lambda2)*(lambda3-lambda2)*(KA-lambda2))+exp(-t*lambda3)*(E3-lambda3)*(E4-lambda3)/((lambda1-lambda3)*(lambda2-lambda3)*(KA-lambda3))+exp(-t*KA)*(E3-KA)*(E4-KA)/((lambda1-KA)*(lambda2-KA)*(lambda3-KA)))
    d$A2[i] = A2term1+A2term2+A2term3   #Amount in the central compartment

    A3term1 = A3last*(exp(-t*lambda1)*(E2-lambda1)*(E4-lambda1)/((lambda2-lambda1)*(lambda3-lambda1))+exp(-t*lambda2)*(E2-lambda2)*(E4-lambda2)/((lambda1-lambda2)*(lambda3-lambda2))+exp(-t*lambda3)*(E2-lambda3)*(E4-lambda3)/((lambda1-lambda3)*(lambda2-lambda3)))
    A3term2 = exp(-t*lambda1)*(I-A2last*k23*lambda1)/((lambda1-lambda2)*(lambda1-lambda3))+exp(-t*lambda2)*(A2last*k23*lambda2-I)/((lambda1-lambda2)*(lambda2-lambda3))+exp(-t*lambda3)*(A2last*k23*lambda3-I)/((lambda1-lambda3)*(lambda3-lambda2))
    A3term3 = A1last*KA*k23*(exp(-t*lambda1)*(E4-lambda1)/((lambda2-lambda1)*(lambda3-lambda1)*(KA-lambda1))+exp(-t*lambda2)*(E4-lambda2)/((lambda1-lambda2)*(lambda3-lambda2)*(KA-lambda2))+exp(-t*lambda3)*(E4-lambda3)/((lambda1-lambda3)*(lambda2-lambda3)*(KA-lambda3))+exp(-t*KA)*(E4-KA)/((lambda1-KA)*(lambda2-KA)*(lambda3-KA)))
    d$A3[i] = A3term1+A3term2+A3term3  #Amount in the first-peripheral compartment

    A4term1 = A4last*(exp(-t*lambda1)*(E2-lambda1)*(E3-lambda1)/((lambda2-lambda1)*(lambda3-lambda1))+exp(-t*lambda2)*(E2-lambda2)*(E3-lambda2)/((lambda1-lambda2)*(lambda3-lambda2))+exp(-t*lambda3)*(E2-lambda3)*(E3-lambda3)/((lambda1-lambda3)*(lambda2-lambda3)))
    A4term2 = exp(-t*lambda1)*(J-A2last*k24*lambda1)/((lambda1-lambda2)*(lambda1-lambda3))+exp(-t*lambda2)*(A2last*k24*lambda2-J)/((lambda1-lambda2)*(lambda2-lambda3))+exp(-t*lambda3)*(A2last*k24*lambda3-J)/((lambda1-lambda3)*(lambda3-lambda2))
    A4term3 = A1last*KA*k24*(exp(-t*lambda1)*(E3-lambda1)/((lambda2-lambda1)*(lambda3-lambda1)*(KA-lambda1))+exp(-t*lambda2)*(E3-lambda2)/((lambda1-lambda2)*(lambda3-lambda2)*(KA-lambda2))+exp(-t*lambda3)*(E3-lambda3)/((lambda1-lambda3)*(lambda2-lambda3)*(KA-lambda3))+exp(-t*KA)*(E3-KA)/((lambda1-KA)*(lambda2-KA)*(lambda3-KA)))
    d$A4[i] = A4term1+A4term2+A4term3  #Amount in the second-peripheral compartment

    A1last = A1last*exp(-t*KA)
    d$A1[i] = A1last + d$AMT[i]*d$F1[i]        #Amount in the absorption compartment

    d$DV[i] <- d$A2[i]/d$V[i]         #Concentration in the absorption compartment

  }
  d
}

#' first-order absorption- 3 compartment-Metabolite
#' @param d data, accepts a NONMEM style data frame for 1 subject with columns for TIME, AMT,MDV,DV, CL, V2, Q3, V3, Q4, V4, KA & F1
#' @return Returns a dataframe with populated columns for A1, A2, A3, A4 and DV
#' @keywords internal
ThreeCompOralMetab <- function(d) {

  # set initial values in the compartments
  d$A1[d$TIME==0] <- d$AMT[d$TIME==0]*d$F1[1]    # Amount in the absorption compartment at time zero.
  d$A2[d$TIME==0] <- 0                   # Amount in the central compartment at time zero.
  d$A3[d$TIME==0] <- 0                   # Amount in the 1st peripheral compartment at time zero.
  d$A4[d$TIME==0] <- 0                   # Amount in the 2nd peripheral compartment at time zero.
  d$Am[d$TIME==0] <- 0                   # Amount in the metabolite compartment at time zero.

  # This loop calculates micro-rate constants based on individual's PK parameter values.
  # It uses these values to calculate macro-rate constants (Lambda1/lambda2/lambda3).
  # Rate constants(micro- and macro), along other parameters, are used in the equations to calculate drug amounts in each compartment.
  # The loop advances the solution from one time interval to the next.
  # It also calculates the concentration in the central compartment.

  for(i in 2:nrow(d)) {

    k20 <- d$CL[i]/d$V[i]
    k23 <- d$Q[i]/d$V[i]
    k32 <- k23*d$V[i]/d$V2[i]
    k24 <- d$Q2[i]/d$V[i]
    k42 <- k24*d$V[i]/d$V3[i]
    km  <- d$km[i]
    kme <- d$CLm[i]/d$Vm[i]
  	KA  <- d$KA[i]
    k30 <- 0
    k40 <- 0
    E2 <- k20+k23+k24+km
    E3 <- k32+k30
    E4 <- k42+k40

    #calculate hybrid rate constants
    a <- E2+E3+E4
    b <- E2*E3+E4*(E2+E3)-k23*k32-k24*k42
    c <- E2*E3*E4-E4*k23*k32-E3*k24*k42

    m <- (3*b - a^2)/3
    n <- (2*a^3 - 9*a*b + 27*c)/27
    Q <- (n^2)/4 + (m^3)/27

    alpha <- sqrt(-1*Q)
    beta <- -1*n/2
    gamma <- sqrt(beta^2+alpha^2)
    theta <- atan2(alpha,beta)

    lambda1 <- a/3 + gamma^(1/3)*(cos(theta/3) + sqrt(3)*sin(theta/3))
    lambda2 <- a/3 + gamma^(1/3)*(cos(theta/3) - sqrt(3)*sin(theta/3))
    lambda3 <- a/3 -(2*gamma^(1/3)*cos(theta/3))

    t <- d$TIME[i]-d$TIME[i-1]
    A1last <- d$A1[i-1]
    A2last <- d$A2[i-1]
    A3last <- d$A3[i-1]
    A4last <- d$A4[i-1]
    Amlast <- d$Am[i-1]

    B = A3last*k32+A4last*k42
    C = E4*A3last*k32+E3*A4last*k42
    I = A2last*k23*E4-A3last*k24*k42+A4last*k23*k42
    J = A2last*k24*E3+A3last*k24*k32-A4last*k23*k32

    A2term1 = A2last*(exp(-t*lambda1)*(E3-lambda1)*(E4-lambda1)/((lambda2-lambda1)*(lambda3-lambda1))+exp(-t*lambda2)*(E3-lambda2)*(E4-lambda2)/((lambda1-lambda2)*(lambda3-lambda2))+exp(-t*lambda3)*(E3-lambda3)*(E4-lambda3)/((lambda1-lambda3)*(lambda2-lambda3)))
    A2term2 = exp(-t*lambda1)*(C-B*lambda1)/((lambda1-lambda2)*(lambda1-lambda3))+exp(-t*lambda2)*(B*lambda2-C)/((lambda1-lambda2)*(lambda2-lambda3))+exp(-t*lambda3)*(B*lambda3-C)/((lambda1-lambda3)*(lambda3-lambda2))
    A2term3 = A1last*KA*(exp(-t*lambda1)*(E3-lambda1)*(E4-lambda1)/((lambda2-lambda1)*(lambda3-lambda1)*(KA-lambda1))+exp(-t*lambda2)*(E3-lambda2)*(E4-lambda2)/((lambda1-lambda2)*(lambda3-lambda2)*(KA-lambda2))+exp(-t*lambda3)*(E3-lambda3)*(E4-lambda3)/((lambda1-lambda3)*(lambda2-lambda3)*(KA-lambda3))+exp(-t*KA)*(E3-KA)*(E4-KA)/((lambda1-KA)*(lambda2-KA)*(lambda3-KA)))
    d$A2[i] = A2term1+A2term2+A2term3

    A3term1 = A3last*(exp(-t*lambda1)*(E2-lambda1)*(E4-lambda1)/((lambda2-lambda1)*(lambda3-lambda1))+exp(-t*lambda2)*(E2-lambda2)*(E4-lambda2)/((lambda1-lambda2)*(lambda3-lambda2))+exp(-t*lambda3)*(E2-lambda3)*(E4-lambda3)/((lambda1-lambda3)*(lambda2-lambda3)))
    A3term2 = exp(-t*lambda1)*(I-A2last*k23*lambda1)/((lambda1-lambda2)*(lambda1-lambda3))+exp(-t*lambda2)*(A2last*k23*lambda2-I)/((lambda1-lambda2)*(lambda2-lambda3))+exp(-t*lambda3)*(A2last*k23*lambda3-I)/((lambda1-lambda3)*(lambda3-lambda2))
    A3term3 = A1last*KA*k23*(exp(-t*lambda1)*(E4-lambda1)/((lambda2-lambda1)*(lambda3-lambda1)*(KA-lambda1))+exp(-t*lambda2)*(E4-lambda2)/((lambda1-lambda2)*(lambda3-lambda2)*(KA-lambda2))+exp(-t*lambda3)*(E4-lambda3)/((lambda1-lambda3)*(lambda2-lambda3)*(KA-lambda3))+exp(-t*KA)*(E4-KA)/((lambda1-KA)*(lambda2-KA)*(lambda3-KA)))
    d$A3[i] = A3term1+A3term2+A3term3

    A4term1 = A4last*(exp(-t*lambda1)*(E2-lambda1)*(E3-lambda1)/((lambda2-lambda1)*(lambda3-lambda1))+exp(-t*lambda2)*(E2-lambda2)*(E3-lambda2)/((lambda1-lambda2)*(lambda3-lambda2))+exp(-t*lambda3)*(E2-lambda3)*(E3-lambda3)/((lambda1-lambda3)*(lambda2-lambda3)))
    A4term2 = exp(-t*lambda1)*(J-A2last*k24*lambda1)/((lambda1-lambda2)*(lambda1-lambda3))+exp(-t*lambda2)*(A2last*k24*lambda2-J)/((lambda1-lambda2)*(lambda2-lambda3))+exp(-t*lambda3)*(A2last*k24*lambda3-J)/((lambda1-lambda3)*(lambda3-lambda2))
    A4term3 = A1last*KA*k24*(exp(-t*lambda1)*(E3-lambda1)/((lambda2-lambda1)*(lambda3-lambda1)*(KA-lambda1))+exp(-t*lambda2)*(E3-lambda2)/((lambda1-lambda2)*(lambda3-lambda2)*(KA-lambda2))+exp(-t*lambda3)*(E3-lambda3)/((lambda1-lambda3)*(lambda2-lambda3)*(KA-lambda3))+exp(-t*KA)*(E3-KA)/((lambda1-KA)*(lambda2-KA)*(lambda3-KA)))
    d$A4[i] = A4term1+A4term2+A4term3

    Amterm1 = Amlast*exp(-t*kme)+km*A2last*(exp(-t*lambda1)*(E3-lambda1)*(E4-lambda1)/((kme-lambda1)*(lambda2-lambda1)*(lambda3-lambda1))+exp(-t*lambda2)*(E3-lambda2)*(E4-lambda2)/((kme-lambda2)*(lambda1-lambda2)*(lambda3-lambda2))+exp(-t*lambda3)*(E3-lambda3)*(E4-lambda3)/((kme-lambda3)*(lambda1-lambda3)*(lambda2-lambda3))+exp(-t*kme)*(E3-kme)*(E4-kme)/((lambda1-kme)*(lambda2-kme)*(lambda3-kme)))
    Amterm2 = km*(exp(-t*lambda1)*(B*lambda1-C)/((lambda1-lambda2)*(lambda1-lambda3)*(lambda1-kme))+exp(-t*lambda2)*(C-B*lambda2)/((lambda1-lambda2)*(lambda2-lambda3)*(lambda2-kme))+exp(-t*lambda3)*(C-B*lambda3)/((lambda1-lambda3)*(lambda3-lambda2)*(lambda3-kme))-exp(-t*kme)*(B*kme-C)/((lambda1-kme)*(kme-lambda2)*(kme-lambda3)))
    Amterm3 = km*A1last*KA*(exp(-t*lambda1)*(E3-lambda1)*(E4-lambda1)/((kme-lambda1)*(lambda2-lambda1)*(lambda3-lambda1)*(KA-lambda1))+exp(-t*lambda2)*(E3-lambda2)*(E4-lambda2)/((kme-lambda2)*(lambda1-lambda2)*(lambda3-lambda2)*(KA-lambda2))+exp(-t*lambda3)*(E3-lambda3)*(E4-lambda3)/((kme-lambda3)*(lambda1-lambda3)*(lambda2-lambda3)*(KA-lambda3))+exp(-t*KA)*(E3-KA)*(E4-KA)/((kme-KA)*(lambda1-KA)*(lambda2-KA)*(lambda3-KA))+exp(-t*kme)*(E3-kme)*(E4-kme)/((lambda1-kme)*(lambda2-kme)*(lambda3-kme)*(KA-kme)))

    d$Am[i] = Amterm1+Amterm2+Amterm3    #Amount in the metabolite compartment


    A1last = A1last*exp(-t*KA)
    d$A1[i] = A1last + d$AMT[i]*d$F1[i]         #Amount in the absoprtion compartment

    d$DV[i] <- d$A2[i]/d$V[i]

  }
  d
}

#' ADVAN-style functions to calculate linear PK systems
#' @param model Standard linear PK model, e.g. `1cmt_iv_bolus`.
#' @param cpp use C++-versions of model (~50x faster than R implementations)
#' @export
#' @return Model function
advan <- function(model, cpp = TRUE) {
  cmt <- list(
    "1cmt_iv_bolus" = 1,
    "2cmt_iv_bolus" = 2,
    "3cmt_iv_bolus" = 3,
    "1cmt_iv_infusion" = 1,
    "2cmt_iv_infusion" = 2,
    "3cmt_iv_infusion" = 3,
    "1cmt_oral" = 2,
    "2cmt_oral" = 3,
    "3cmt_oral" = 4
  )
  type <- list(
    "1cmt_iv_bolus" = "bolus",
    "2cmt_iv_bolus" = "bolus",
    "3cmt_iv_bolus" = "bolus",
    "1cmt_iv_infusion" = "infusion",
    "2cmt_iv_infusion" = "infusion",
    "3cmt_iv_infusion" = "infusion",
    "1cmt_oral" = "oral",
    "2cmt_oral" = "oral",
    "3cmt_oral" = "oral"
  )
  if(cpp) {
    mods <- list(
      "1cmt_iv_bolus" = pk_1cmt_iv_bolus,
      "2cmt_iv_bolus" = pk_2cmt_iv_bolus,
      "3cmt_iv_bolus" = pk_3cmt_iv_bolus,
      "1cmt_iv_infusion" = pk_1cmt_iv_infusion,
      "2cmt_iv_infusion" = pk_2cmt_iv_infusion,
      "3cmt_iv_infusion" = pk_3cmt_iv_infusion,
      "1cmt_oral" = pk_1cmt_oral,
      "2cmt_oral" = pk_2cmt_oral,
      "3cmt_oral" = pk_3cmt_oral
    )
  } else {
    mods <- list(
      "1cmt_iv_bolus" = OneCompIVbolus,
      "2cmt_iv_bolus" = TwoCompIVbolus,
      "3cmt_iv_bolus" = ThreeCompIVbolus,
      "1cmt_iv_infusion" = OneCompIVinfusion,
      "2cmt_iv_infusion" = TwoCompIVinfusion,
      "3cmt_iv_infusion" = ThreeCompIVinfusion,
      "1cmt_oral" = OneCompOral,
      "2cmt_oral" = TwoCompOral,
      "3cmt_oral" = ThreeCompOral
    )
  }
  m <- mods[[model]]
  if(is.null(m)) {
    stop(paste0("Model (", model, ") not found!"))
  }
  attr(m, "cmt") <- cmt[[model]]
  attr(m, "type") <- type[[model]]
  attr(m, "implementation") <- ifelse0(cpp, "c++", "R")
  return(m)
}
