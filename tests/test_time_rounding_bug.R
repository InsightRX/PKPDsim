library(testit)
library(PKPDsim)
Sys.setenv("R_TESTS" = "")

## time-rounding bug 20170804
p <- list(CL = 5,
          V  = 50,
          Q = 10,
          V2 = 150
)
r1 <- new_regimen(amt = 100,
                  times = c(0, 24, 36),
                  type = "infusion")
mod <- new_ode_model(model = "pk_2cmt_iv", cpp_show_code = F)
dat1 <- sim_ode (ode = mod, parameters = p, regimen = r1, t_obs=seq(from=0, to=150, len=100))
testit::assert("Should have no NAs", !any(is.na(dat1$y)))
