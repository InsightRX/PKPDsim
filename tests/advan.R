library(testit)
library(PKPDsim)
Sys.setenv("R_TESTS" = "")

## These models are also tested in the unit tests for `calc_ss_analytics()`, so just testing a few example cases here

dose <- 100
interval <- 12
t_inf <- 1
n_days <- 5
parameters <- list(CL = 10, V = 50, KA = 0.5, Q = 5, V2 = 100, Q2 = 3, V3 = 150, F1 = 1)
t_obs <- c(3, 6, 8, 23)
reg_bolus <- new_regimen(amt = dose,
                            times = seq(0, interval * n_days * (24/interval), interval),
                            t_inf = t_inf, type = "bolus")
data <- advan_create_data(reg_bolus,
                          parameters = parameters,
                          cmts = 5,
                          t_obs = t_obs)
check_equal <- function(data1, data2, tol = 1e-6) {
  all(abs(as.numeric(unlist(data1)) - as.numeric(unlist(data2))) < tol)
}

## Infusion dataset
reg_infusion <- new_regimen(amt = dose,
                            times = seq(0, interval * n_days * (24/interval), interval),
                            t_inf = t_inf, type = "infusion")
data_infusion <- advan_create_data(reg_infusion,
                          parameters = parameters,
                          cmts = 6,
                          t_obs = t_obs)

## One compartment
res1_iv   <- advan("1cmt_iv_bolus", cpp=FALSE)(data)
res1_iv_c <- advan("1cmt_iv_bolus", cpp=TRUE)(data)
assert("1cmt_iv_bolus", round(res1_iv[res1_iv$TIME == 23,]$DV, 3) == 0.242)
assert("1cmt_iv_bolus: exact same output from C function", check_equal(res1_iv, res1_iv_c))
assert("1cmt_iv_bolus: no NA", !any(is.na(res1_iv$DV)))

res1_iv_inf   <- advan("1cmt_iv_infusion", cpp=FALSE)(data_infusion)
res1_iv_inf_c <- advan("1cmt_iv_infusion", cpp=TRUE)(data_infusion)
f <- advan("1cmt_iv_infusion", cpp=FALSE)
f <- advan("1cmt_iv_infusion", cpp=TRUE)
assert("1cmt_iv_infusion", round(res1_iv_inf[res1_iv_inf$TIME == 23,]$DV, 3) == 0.268)
assert("1cmt_iv_infusion: exact same output from C function", check_equal(res1_iv_inf, res1_iv_inf_c))
assert("1cmt_iv_infusion: no NA", !any(is.na(res1_iv_inf$DV)))

res1_oral <- advan("1cmt_oral", cpp=FALSE)(data)
res1_oral_c <- advan("1cmt_oral", cpp=TRUE)(data)
assert("1cmt_oral", round(res1_oral[res1_oral$TIME == 23,]$DV, 3) == 0.389)
assert("1cmt_iv_bolus: exact same output from C function", check_equal(res1_oral, res1_oral_c))
assert("1cmt_oral no NA", !any(is.na(res1_oral$DV)))

## Two compartment
res2_iv   <- advan("2cmt_iv_bolus", cpp=FALSE)(data)
res2_iv_c <- advan("2cmt_iv_bolus", cpp=TRUE)(data)
assert("2cmt_iv_bolus", round(res2_iv[res2_iv$TIME == 23,]$DV, 3) == 0.212)
assert("2cmt_iv_bolus: exact same output from C function", check_equal(res2_iv, res2_iv_c))
assert("2cmt_iv_bolus: no NA", !any(is.na(res2_iv$DV)))

# data_inf <- create_advan_data(regimen = reg_infusion, parameters = parameters)
res2_iv_inf   <- advan("2cmt_iv_infusion", cpp=FALSE)(data_infusion)
res2_iv_inf_c <- advan("2cmt_iv_infusion", cpp=TRUE)(data_infusion)
assert("2cmt_iv_infusion", round(res2_iv_inf[res2_iv_inf$TIME == 23,]$DV, 3) == 0.225)
assert("2cmt_iv_infusion: exact same output from C function", check_equal(res2_iv_inf, res2_iv_inf_c))
assert("2cmt_iv_infusion: no NA", !any(is.na(res2_iv_inf$DV)))

res2_oral <- advan("2cmt_oral", cpp=FALSE)(data)
res2_oral <- advan("2cmt_oral", cpp=TRUE)(data)
assert("2cmt_oral", round(res2_oral[res2_oral$TIME == 23,]$DV, 3) == 0.302)
assert("2cmt_oral: exact same output from C function", check_equal(res2_oral, res2_oral))
assert("2cmt_oral no NA", !any(is.na(res2_oral$DV)))

# f1 <- function() {res2_iv   <- advan("2cmt_iv_bolus"]](data)}
# f2 <- function() {res2_iv_c <- PKPDsim:::pk_2cmt_iv_bolus(data)}
# microbenchmark::microbenchmark(f1(), f2(), times = 100)

## Three compartment
res3_iv   <- advan("3cmt_iv_bolus", cpp=FALSE)(data)
res3_iv_c <- advan("3cmt_iv_bolus", cpp=TRUE)(data)
assert("3cmt_iv", round(res3_iv[res3_iv$TIME == 23,]$DV, 3) == 0.169)
assert("3cmt_iv_bolus: exact same output from C function", check_equal(res3_iv, res3_iv_c))
assert("3cmt_iv no NA", !any(is.na(res3_iv$DV)))

res3_iv_inf   <- advan("3cmt_iv_infusion", cpp=FALSE)(data_infusion)
res3_iv_inf_c <- advan("3cmt_iv_infusion", cpp=TRUE)(data_infusion)
assert("3cmt_iv_infusion", round(res3_iv[res3_iv$TIME == 23,]$DV, 3) == 0.169) ## CHECK this value!!!
assert("3cmt_iv_infusion: exact same output from C function", check_equal(res3_iv_inf, res3_iv_inf_c))
assert("3cmt_iv_infusion no NA", !any(is.na(res3_iv$DV)))

res3_oral <- advan("3cmt_oral", cpp=FALSE)(data)
res3_oral_c <- advan("3cmt_oral", cpp=TRUE)(data)
assert("3cmt_oral", round(res3_oral[res3_oral$TIME == 23,]$DV, 3) == 0.236)
assert("3cmt_oral: exact same output from C function", check_equal(res3_oral, res3_oral_c))
assert("3cmt_oral no NA", !any(is.na(res3_oral$DV)))
