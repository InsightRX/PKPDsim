library(testit)
library(PKPDsim)
Sys.setenv("R_TESTS" = "")

reg1 <- new_regimen(amt = 1000, times = c(0, 24), type = "infusion", t_inf = 1)
reg2 <- new_regimen(amt = 500, times = c(12, 36), type = "oral")
reg3 <- merge_regimen(regimens = list(reg1, reg2))
assert("correct merge", all(c(reg3$dose_times == c(0, 12, 24, 36),
                              reg3$type == c("infusion", "oral", "infusion", "oral"),
                              reg3$dose_amts == c(1000, 500, 1000, 500),
                              reg3$t_inf == c(1, 0, 1, 0) ) ))
