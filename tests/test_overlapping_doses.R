library(PKPDsim)
library(testit)
Sys.setenv("R_TESTS" = "")

mod <- new_ode_model(model = "pk_1cmt_iv")
reg <- new_regimen(amt = c(100, 100, 100, 100),
                   times = c(0, 6, 7, 12, 13),
                   type = "infusion",
                   t_inf = c(2, 2, 2, 2, 2))
par <- list(CL = 5, V = 50)
res <- sim_ode(mod, par = par, reg = reg, only_obs = TRUE)
# assert(res[res$t == 7,]$y > res[res$t == 6,]$y)
# assert(res[res$t == 8,]$y > res[res$t == 7,]$y)
# assert(res[res$t == 9,]$y > res[res$t == 8,]$y)
# assert(diff(res[res$t %in% c(7,8),]$y) > diff(res[res$t %in% c(8,9),]$y))

## Test adjoining (=continuous) doses
reg2 <- new_regimen(amt = c(100, 100, 100, 100),
                   times = c(0, 6, 12, 18),
                   type = "infusion",
                   t_inf = c(2, 6, 2, 6))
res2 <- sim_ode(mod, par = par, reg = reg2, only_obs = TRUE)
assert("adjoining infusion simulated correctly",
       res2[res2$t == 12,]$y > res2[res2$t == 6,]$y &&
       res2[res2$t == 14,]$y > res2[res2$t == 12,]$y &&
       res2[res2$t == 18,]$y < res2[res2$t == 14,]$y)
#
# library(ggplot2)
# ggplot(res2, aes(x = t, y = y)) + geom_line()
