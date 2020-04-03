library(testit)
library(PKPDsim)
Sys.setenv("R_TESTS" = "")

## These models are also tested in the unit tests for `calc_ss_analytics()`, so just testing a few example cases here

dose <- 100
interval <- 12
n_days <- 5
parameters <- list(CL = 10, V = 50, KA = 0.5, Q = 5, V2 = 100, Q2 = 3, V3 = 150)
t_obs <- c(3, 6, 8, 23)

data <- data.frame(ID = 1,
                TIME = seq(0, interval * n_days * (24/interval), interval),
                AMT = dose, EVID = 1, DV = 0,
                CL = parameters$CL,
                V = parameters$V,
                Q  = ifelse0(parameters$Q, NA),
                V2 = ifelse0(parameters$V2, NA),
                Q2  = ifelse0(parameters$Q2, NA),
                V3 = ifelse0(parameters$V3, NA),
                KA = ifelse0(parameters$KA, NA),
                A1 = 0, A2 = 0, A3 = 0, A4 = 0, A5 = 0,
                F1 = 1)
obs <- tail(data, 1)
obs$AMT <- 0
obs$EVID <- 0
for(i in seq(t_obs)) {
  obs$TIME <- t_obs[i]
  data <- rbind(data, obs)
}
data <- data[order(data$TIME, -data$EVID),]

res1_oral <- advan_funcs[["1cmt_oral"]](data)
assert("1cmt_oral", round(res1_oral[res1_oral$TIME == 23,]$DV, 3) == 0.389)
assert("1cmt_oral no NA", !any(is.na(res1_oral$DV)))

res1_iv   <- advan_funcs[["1cmt_iv_bolus"]](data)
assert("1cmt_iv_bolus", round(res1_iv[res1_iv$TIME == 23,]$DV, 3) == 0.242)
assert("1cmt_iv_bolus no NA", !any(is.na(res1_oral$DV)))

res2_oral <- advan_funcs[["2cmt_oral"]](data)
assert("2cmt_oral", round(res2_oral[res2_oral$TIME == 23,]$DV, 3) == 0.302)
assert("2cmt_oral no NA", !any(is.na(res2_oral$DV)))

res2_iv   <- advan_funcs[["2cmt_iv_bolus"]](data)
assert("2cmt_iv", round(res2_iv[res2_iv$TIME == 23,]$DV, 3) == 0.212)
assert("2cmt_iv no NA", !any(is.na(res2_iv$DV)))

res3_oral <- advan_funcs[["3cmt_oral"]](data)
assert("3cmt_oral", round(res3_oral[res3_oral$TIME == 23,]$DV, 3) == 0.236)
assert("3cmt_oral no NA", !any(is.na(res3_oral$DV)))

res3_iv   <- advan_funcs[["3cmt_iv_bolus"]](data)
assert("3cmt_iv", round(res3_iv[res3_iv$TIME == 23,]$DV, 3) == 0.169)
assert("3cmt_iv no NA", !any(is.na(res3_iv$DV)))
