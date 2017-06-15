library(PKPDsim)
library(PKPDplot)
library(dplyr)
library(testit)

covs <- list(
  CRCL = new_covariate(value = c(4.662744, 5.798767, 6.195943),
                       times = c(0, 18, 29), implementation = "locf")
)
mod <- new_ode_model(
  code = "
    dAdt[1] = -(Q/V)*A[1] + (Q/V2)*A[2] -(CL/V)*A[1];
    dAdt[2] = -(Q/V2)*A[2] + (Q/V)*A[1];
 ",
  "pk_code" = "
    CLi = CL + CRCL
  ",
  obs = list (cmt = 2, scale = "V"),
  covariates = covs, declare_variables = "CLi")
par <- list(CL = 3, V = 50, Q = 2.5, V2 = 70)
reg <- new_regimen(amt = 1000, n = 2, interval = 11.9, type = 'infusion',t_inf = 1)
t_obs <- seq(0, 48, 12)

sim1 <- sim_ode(mod, parameters = par, regimen = reg, return_design = TRUE,
                covariates = covs, only_obs = TRUE, t_obs = t_obs)
sim1 <- sim_ode(mod, parameters = par, regimen = reg,
        covariates = covs, only_obs = TRUE, t_obs = t_obs) %>%
  filter(t == 48)
sim2 <- sim_ode(mod, parameters = par, regimen = reg,
        covariates = covs, only_obs = TRUE, t_obs = 48)

assert("timevarying ok", round(sim1$y,3) == round(sim2$y,3))
