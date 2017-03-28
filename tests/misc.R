library(testit)
library(PKPDsim)
library(PKPDplot)

## Joining regimens:
tmp_join <- new_covariate(value = c(1,2,3,4,5), times=c(0,.3,.5,1.7,1.9))
assert("joining works", length(tmp_join$times) == 2 && length(tmp_join$value) == 2)

## Test rate argument:
reg1 <- new_regimen(amt = 100, times = c(0, 12, 24, 36, 48),
                   type = "infusion", rate = c(1,2,3,4,5))
assert("Rate works", round(reg1$t_inf) == c(100, 50, 33, 25, 20))
reg2 <- new_regimen(amt = 100, times = c(0, 12, 24, 36, 48),
                   type = "infusion", rate = c(5))
assert("Rate works", all(round(reg2$t_inf) == 20))

## mixed bolus and infusion
reg3 <- new_regimen(amt = 100, times = c(0, 12, 24, 36, 48),
                    t_inf = c(3),
                    type= c("bolus", "infusion", "bolus", "infusion", "bolus"))
mod <- new_ode_model("pk_1cmt_iv")
dat <- sim_ode(ode = mod, regimen = reg3, parameters = list(CL = 5, V = 50))
