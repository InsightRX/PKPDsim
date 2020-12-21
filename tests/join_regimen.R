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

res1 <- join_regimen(reg1, reg2, t_dose_update = 65) # early update
res2 <- join_regimen(reg1, reg2, interval = 15) # early update
res3 <- join_regimen(reg1, reg2, t_dose_update = 100) # gap
res4 <- join_regimen(reg1, reg2, t_dose_update = 0) # don't keep regimen1

assert("t_inf correctly modified using t_dose_update", all(res1$t_inf == c(24, 24, 17, 24, 24, 24) ))
assert("dose corrected for 3rd dose", round(res1$dose_amts[3],1) == round(100*17/28,1))
assert("t_inf correctly modified using interval", all(res2$t_inf == c(24, 24, 15, 24, 24, 24) ))
assert("dose corrected for 3rd dose", round(res2$dose_amts[3],1) == round(100*15/28,1))
assert("t_inf/amt not modified", all(res3$t_inf == c(24, 24, 28, 24, 24, 24) ))
assert("t_inf/amt not modified", all(res3$amt == c(100, 100, 100, 200, 200, 200) ))
assert("when t_update = 0, should be equal to reg2", all(res4$dose_amts == reg2$dose_amts))
