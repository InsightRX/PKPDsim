library(testit)
library(PKPDsim)
Sys.setenv("R_TESTS" = "")
rdelta <- function(x, y) { (x - y) / x }

dose <- 100
interval <- 12
n_days <- 5
t_inf <- 1.5
parameters <- list(CL = 10, V = 50, KA = 0.5, Q = 5, V2 = 100, Q2 = 3, V3 = 150, F1 = 1)
t_obs <- c(3, 6, 8, 23, 47)

## ODE models for testing
mod_1cmt <- new_ode_model(code="dAdt[1] = -(CL/V)*A[1]; dAdt[2] = A[1]/V;", parameters = parameters)
mod_2cmt <- new_ode_model(code="dAdt[1] = -(CL/V)*A[1] - (Q/V)*A[1] + (Q/V2)*A[2]; dAdt[2] = +(Q/V)*A[1] - (Q/V2)*A[2]; dAdt[3] = A[1]/V;", parameters = parameters)

## bolus dataset
reg_bolus <- new_regimen(amt = dose,
                         times = seq(0, interval * n_days * (24/interval), interval),
                         t_inf = t_inf, type = "bolus")
data_bolus <- advan_create_data(reg_bolus,
                          parameters = parameters,
                          cmts = 5, t_obs = t_obs)

## Infusion dataset
reg_infusion <- new_regimen(amt = dose,
                            times = seq(0, interval * n_days * (24/interval), interval),
                            t_inf = t_inf, type = "infusion")
data_infusion <- advan_create_data(reg_infusion,
                                   parameters = parameters,
                                   cmts = 6, t_obs = t_obs)

## One compartment bolus
res1_iv_r <- advan("1cmt_iv_bolus", cpp=FALSE)(data_bolus)
res1_iv_c <- advan("1cmt_iv_bolus", cpp=TRUE)(data_bolus)
res1_iv_ode <- sim(ode = mod_1cmt, regimen = reg_bolus, parameters = parameters, t_obs = t_obs)
assert("AUC-R matches ODE sim", all(rdelta(res1_iv_r[res1_iv_r$TIME %in% t_obs,]$AUC, res1_iv_ode[res1_iv_ode$comp == 2,]$y) < 1e-5))
assert("AUC-C matches ODE sim", all(rdelta(res1_iv_c[res1_iv_c$TIME %in% t_obs,]$AUC, res1_iv_ode[res1_iv_ode$comp == 2,]$y) < 1e-5))

## One compartment infusion
res1_inf_r <- advan("1cmt_iv_infusion", cpp=FALSE)(data_infusion)
res1_inf_c <- advan("1cmt_iv_infusion", cpp=TRUE)(data_infusion)
res1_inf_ode <- sim(ode = mod_1cmt, regimen = reg_infusion, parameters = parameters, t_obs = t_obs)
assert("AUC-R matches ODE sim", all(rdelta(res1_inf_r[res1_inf_r$TIME %in% t_obs,]$AUC, res1_inf_ode[res1_inf_ode$comp == 2,]$y) < 1e-5))
assert("AUC-C matches ODE sim", all(rdelta(res1_inf_c[res1_inf_c$TIME %in% t_obs,]$AUC, res1_inf_ode[res1_inf_ode$comp == 2,]$y) < 1e-5))

## Two compartment bolus
res2_iv_r <- advan("2cmt_iv_bolus", cpp=FALSE)(data_infusion)
res2_iv_c <- advan("2cmt_iv_bolus", cpp=TRUE)(data_infusion)
res2_iv_ode <- sim(ode = mod_2cmt, regimen = reg_bolus, parameters = parameters, t_obs = t_obs)
assert("AUC-R matches ODE sim", all(rdelta(res2_iv_r[res2_iv_r$TIME %in% t_obs,]$AUC, res2_iv_ode[res2_iv_ode$comp == 2,]$y) < 1e-5))
assert("AUC-C matches ODE sim", all(rdelta(res2_iv_c[res2_iv_c$TIME %in% t_obs,]$AUC, res2_iv_ode[res2_iv_ode$comp == 2,]$y) < 1e-5))

## Two compartment infusion
res2_inf_r <- advan("2cmt_iv_infusion", cpp=FALSE)(data_infusion)
res2_inf_c <- advan("2cmt_iv_infusion", cpp=TRUE)(data_infusion)
res2_inf_ode <- sim(ode = mod_2cmt, regimen = reg_infusion, parameters = parameters, t_obs = t_obs)
assert("AUC-R matches ODE sim", all(rdelta(res2_inf_r[res2_inf_r$TIME %in% t_obs,]$AUC, res2_inf_ode[res2_inf_ode$comp == 2,]$y) < 1e-5))
assert("AUC-C matches ODE sim", all(rdelta(res2_inf_c[res2_inf_c$TIME %in% t_obs,]$AUC, res2_inf_ode[res2_inf_ode$comp == 2,]$y) < 1e-5))
