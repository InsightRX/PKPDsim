library(testit)
library(PKPDsim)
Sys.setenv("R_TESTS" = "")

reg1 <- new_regimen(amt = c(100, 100, 100),
                    t_inf = c(24, 24, 28),
                    time = c(0, 24, 48),
                         type = "infusion")
reg2 <- new_regimen(amt = c(200, 200, 200),
                    t_inf = c(24,24,24),
                    time = c(0, 24, 48),
                    type = "infusion")

res1 <- join_regimen(reg1, reg2, t_dose_update = 65)
res2 <- join_regimen(reg1, reg2, interval = 15)

assert("t_inf correctly modified using t_dose_update", all(res1$t_inf == c(24, 24, 17, 24, 24, 24) ))
assert("t_inf correctly modified using interval", all(res2$t_inf == c(24, 24, 15, 24, 24, 24) ))
