library(testit)
library(PKPDsim)
Sys.setenv("R_TESTS" = "")

reg1 <- new_regimen(amt = 1000, n = 12, interval = 12, type = "oral")

## Using parameters
lag1 <- apply_lagtime(regimen = reg1, lagtime = "TLAG", parameters = list(CL = 5, V = 50, TLAG = .5))
lag2 <- apply_lagtime(regimen = reg1, lagtime = .75)

assert("lagtime applied", lag1$dose_times == reg1$dose_times + .5)
assert("lagtime applied", lag2$dose_times == reg1$dose_times + .75)
