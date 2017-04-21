library(PKPDsim)
library(testit)
Sys.setenv("R_TESTS" = "")

parameters <- list(KA = 0.5, CL = 5, V = 50)
covs <- list(WT = new_covariate(50))
reg <- new_regimen(amt = 100, n = 1, interval = 12, type = "bolus")
mod1 <- new_ode_model(code = "
  CLi = CL * pow(WT/70, 0.75)
  dAdt[1] = -KA * A[1]
  dAdt[2] = KA*A[1] - (CLi/V)*A[2]
", dose = list(cmt = 1, bioav = 1), covariates = covs, parameters = parameters)
mod2 <- new_ode_model(code = "
  CLi = CL * pow(WT/70, 0.75)
  dAdt[1] = -KA * A[1]
  dAdt[2] = KA*A[1] - (CLi/V)*A[2]
", dose = list(cmt = 1, bioav = 0.5), covariates = covs, parameters = parameters)
mod3 <- new_ode_model(code = "
  CLi = CL * pow(WT/70, 0.75)
  dAdt[1] = -KA * A[1]
  dAdt[2] = KA*A[1] - (CLi/V)*A[2]
", dose = list(cmt = 1, bioav = "WT/70"), covariates = covs, parameters = parameters)
y1 <- sim_ode(mod1, parameters = parameters, regimen = reg, covariates = covs, only_obs=TRUE)$y
y2 <- sim_ode(mod2, parameters = parameters, regimen = reg, covariates = covs, only_obs=TRUE)$y
y3 <- sim_ode(mod3, parameters = parameters, regimen = reg, covariates = covs, only_obs=TRUE)$y

assert("bioav numeric option working", all(round(y1,1) == round(y2*2, 1)))
assert("bioav string option working", all(round(y1*(50/70),1) == round(y3, 1)))
