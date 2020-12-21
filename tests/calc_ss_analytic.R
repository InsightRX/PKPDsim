library(testit)
library(PKPDsim)
Sys.setenv("R_TESTS" = "")

## INFUSION MODELS AND MODELS WITH METABOLITES ARE CURRENTLY NOT SUPPORTED YET, WILL FIX IN SEPARATE PR!

dose <- 100
interval <- 12
n_ss <- 100
reg_oral <- new_regimen(amt = dose, interval = interval, n = n_ss, type = "oral")
reg_bolus <- new_regimen(amt = dose, interval = interval, n = n_ss, type = "bolus")
reg_inf <- new_regimen(amt = dose, interval = interval, n = n_ss, type = "infusion")
t_obs <- max(reg_oral$dose_times) + interval

delta <- function(x, ref) { abs(x-ref)/ref }

## 1 cmt oral
par <- list(CL = 5, V = 100, KA = 1)
pk_1cmt_oral <- new_ode_model("pk_1cmt_oral")
res_ana <- calc_ss_analytic(f = "1cmt_oral", dose = dose, interval = interval, parameters = par)
res_ode <- sim(pk_1cmt_oral, parameters = par, regimen = reg_oral, t_obs = t_obs, only_obs = F)$y
assert("1cmt oral ss correctly number of cmts", attr(advan("1cmt_oral"), "cmt") == 2)
assert("1cmt oral ss correct", max(delta(res_ana[1:2], res_ode[1:2])) < 0.001)

## Test correct adding of extra compartments
res_ana_transit <- calc_ss_analytic(f = "1cmt_oral", dose = dose, interval = interval, parameters = par, n_transit_compartments = 3)
res_ana_auc <- calc_ss_analytic(f = "1cmt_oral", dose = dose, interval = interval, parameters = par, auc = TRUE)
res_ana_both <- calc_ss_analytic(f = "1cmt_oral", dose = dose, interval = interval, parameters = par, auc = TRUE, n_transit_compartments = 3)
assert("correct adding of transit comps", length(res_ana_transit) == 5)
assert("correct adding of AUC comp", length(res_ana_auc) == 3)
assert("correct adding of transit comps + AUC comp (1)", length(res_ana_both) == 6)
assert("correct adding of transit comps + AUC comp (2)", sum(res_ana_both[c(2,3,6)]) == 0)

## 1 cmt iv bolus
par <- list(CL = 5, V = 100)
pk_1cmt_iv <- new_ode_model("pk_1cmt_iv")
res_ana <- calc_ss_analytic(f = "1cmt_iv_bolus", dose = dose, interval = interval, parameters = par)
res_ode <- sim(pk_1cmt_iv, parameters = par, regimen = reg_bolus, t_obs = t_obs, only_obs = F, duplicate_t_obs = FALSE)$y
assert("1cmt oral ss correct number of cmts", attr(advan("1cmt_iv_bolus"), "cmt") == 1)
assert("1cmt iv bolus ss correct", max(delta(res_ana[1], res_ode[1])) < 0.001)

## 1 cmt iv infusion
res_ana <- calc_ss_analytic(f = "1cmt_iv_infusion", dose = dose, interval = interval, t_inf = 1, parameters = par)
res_ode <- sim(pk_1cmt_iv, parameters = par, regimen = reg_inf, t_obs = t_obs, only_obs = F)$y
assert("1cmt iv correct number of cmts", attr(advan("1cmt_iv_bolus"), "cmt") == 1)
assert("1cmt iv infusion ss correct", max(delta(res_ana[1], res_ode[1])) < 0.001)

## 2 cmt oral
par <- list(CL = 5, V = 100, Q = 3, V2 = 150, KA = 1)
pk_2cmt_oral <- new_ode_model("pk_2cmt_oral")
res_ana <- calc_ss_analytic(f = "2cmt_oral", dose = dose, interval = interval, parameters = par)
res_ode <- sim(pk_2cmt_oral, parameters = par, regimen = reg_oral, t_obs = t_obs, only_obs = F)$y
assert("2cmt oral ss correct number of cmts", attr(advan("2cmt_oral"), "cmt") == 3)
assert("2cmt oral ss correct result", max(delta(res_ana[1:3], res_ode[1:3])) < 0.001)

## 2 cmt iv bolus
par <- list(CL = 5, V = 100, Q = 3, V2 = 150)
pk_2cmt_iv <- new_ode_model("pk_2cmt_iv")
res_ana <- calc_ss_analytic(f = "2cmt_iv_bolus", dose = dose, interval = interval, parameters = par)
res_ode <- sim(pk_2cmt_iv, parameters = par, regimen = reg_bolus, t_obs = t_obs, only_obs = F)$y
assert("2cmt iv bolus correct number of cmts", attr(advan("2cmt_iv_bolus"), "cmt") == 2)
assert("2cmt iv bolus ss correct", max(delta(res_ana[1], res_ode[1])) < 0.001)

## 2 cmt iv infusion
res_ana <- calc_ss_analytic(f = "2cmt_iv_infusion", dose = dose, interval = interval, parameters = par, t_inf = 1)
res_ode <- sim(pk_2cmt_iv, parameters = par, regimen = reg_inf, t_obs = t_obs, only_obs = F)$y
assert("2cmt iv bolus correct number of cmts", attr(advan("2cmt_iv_bolus"), "cmt") == 2)
assert("2cmt iv bolus ss correct", max(delta(res_ana[1], res_ode[1])) < 0.001)

## 3 cmt oral
par <- list(CL = 5, V = 100, Q = 3, V2 = 150, Q2 = 6, V3 = 250, KA = 1)
pk_3cmt_oral <- new_ode_model("pk_3cmt_oral")
res_ana <- calc_ss_analytic(f = "3cmt_oral", dose = dose, interval = interval, parameters = par)
res_ode <- sim(pk_3cmt_oral, parameters = par, regimen = reg_oral, t_obs = t_obs, only_obs = F)$y
assert("3cmt oral ss correct number of cmts", attr(advan("3cmt_oral"), "cmt") == 4)
assert("3cmt oral ss correct result", max(delta(res_ana[1:3], res_ode[1:3])) < 0.001)

## 3 cmt iv bolus
par <- list(CL = 5, V = 100, Q = 3, V2 = 150, Q2 = 6, V3 = 250)
pk_3cmt_iv <- new_ode_model("pk_3cmt_iv")
res_ana <- calc_ss_analytic(f = "3cmt_iv_bolus", dose = dose, interval = interval, parameters = par)
res_ode <- sim(pk_3cmt_iv, parameters = par, regimen = reg_bolus, t_obs = t_obs, only_obs = F)$y
assert("3cmt iv bolus correct number of cmts", attr(advan("3cmt_iv_bolus"), "cmt") == 3)
assert("3cmt iv bolus ss correct", max(delta(res_ana[1], res_ode[1])) < 0.001)

## 3 cmt iv infusion
res_ana <- calc_ss_analytic(f = "3cmt_iv_infusion", dose = dose, interval = interval, parameters = par, t_inf = 1)
res_ode <- sim(pk_3cmt_iv, parameters = par, regimen = reg_inf, t_obs = t_obs, only_obs = F)$y
assert("3cmt iv bolus correct number of cmts", attr(advan("3cmt_iv_bolus"), "cmt") == 3)
assert("3cmt iv bolus ss correct", max(delta(res_ana[1], res_ode[1])) < 0.001)
