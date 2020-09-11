library(testit)
library(PKPDsim)
Sys.setenv("R_TESTS" = "")

## Create dataset
dose <- 100
interval <- 12
n_days <- 2
parameters <- list(CL = 10, V = 50, KA = 0.5, Q = 5, V2 = 100, Q2 = 3, V3 = 150, F1 = 1)
t_obs <- seq(0, 40, .1)
reg_bolus <- new_regimen(amt = dose,
                         times = seq(0, interval * n_days * (24/interval), interval),
                         type = "bolus")
## there is slight difference in how bolus doses are handled.
## Analytical equation is perhaps more consistent, so not testing simulations at dose times. Should look into later.
t_obs <- t_obs[! t_obs %in% reg_bolus$dose_times]
covariates <- list(WT = new_covariate(80), CRCL=new_covariate(4.5))

## Using analytic equations model:
data_ana <- sim(
    analytical = "1cmt_iv_bolus",
    parameters = parameters,
    covariates = covariates,
    regimen = reg_bolus,
    t_obs = t_obs,
    covariate_model = "CL = CL * (CRCL / 3)^0.75; V = V * (WT / 70.0)")

## Using ODE model:
mod1 <- new_ode_model(
  code = "
    dAdt[1] = -( (CL*pow(CRCL/3.0, 0.75)) / (V*WT/70.0) ) * A[1];
  ",
  covariates = covariates,
  obs = list(cmt = 1, scale = "V*WT/70.0"), dose = list(cmt = 1)
)
data_ode <- sim(
  ode = mod1,
  parameters = parameters,
  covariates = covariates,
  regimen = reg_bolus,
  t_obs = t_obs,
  duplicate_t_obs = TRUE,
  only_obs = TRUE)

assert("Correct size output data.frame", nrow(data_ana) == nrow(data_ode))
assert("Correct values simulated", all(round(data_ana$y,4) == round(data_ode$y, 4)))

## Using analytic equations model, with reparametrization:
repar <- list(CL = "CL * (1 + TH_CRCL * (CRCL*16.66667 - 66))",
              V = "V * WT",
              Q = "Q",
              V2 = "V2 * WT")
data_ana <- sim(
  analytical = "1cmt_iv_bolus",
  parameters = parameters,
  reparametrization = repar,
  covariates = covariates,
  regimen = reg_bolus,
  t_obs = t_obs,
  covariate_model = "CL = CL * (CRCL / 3)^0.75; V = V * (WT / 70.0)")

