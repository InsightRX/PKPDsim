# install_github("ronkeizer/PKPDsim")
library(PKPDsim)
library(ggplot2)

p <- list(CL = 38.48,
          V  = 7.4,
          KA = .3,
          K0 = 4,
          K0_T = 1)

r1 <- new_regimen(amt = 100,
                  interval = 24,
                  n = 10)

omega <- cv_to_omega (list(CL = 0.3,
                           V = 0.3,
                           KA = 0.1,
                           K0 = .1,
                           K0_T = .3), p)

## sequential k0 and ka
sim_ode_shiny(ode = "pk_1cmt_oral_sequential",
              par = p,
              regimen = new_regimen(amt=30),
              omega = omega)

## parallel k0 and ka
sim_ode_shiny(ode = "pk_1cmt_oral_parallel",
              par = p,
              regimen = new_regimen(amt=30),
              omega = omega)

## transit compartment model (tip: set view to "untransformed")
p_tr <- list(CL = 17.2,
             V  = 45.1,
             KA = 0.38,
             MTT = 2,
             N = 20.1,
             F = 0) # don't forget to set F=0!

omega <- cv_to_omega (list(CL = 0.3,
                           V = 0.3,
                           KA = 0.1,
                           N = 0.3,
                           MTT = 0.3), p_tr)

sim_ode_shiny(ode = "pk_1cmt_oral_transit",
              par = p_tr,
              regimen = new_regimen(amt=100, interval=24, n = 1),
              omega = omega)
