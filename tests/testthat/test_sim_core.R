library(PKPDsim)
library(testit)
Sys.setenv("R_TESTS" = "")

pk1 <- new_ode_model(code = "
  dAdt[1] = -KA * A[1]
  dAdt[2] = +KA * A[1] -(CL/V) * A[2]
", obs = list(cmt = 2) )

reg <- new_regimen(amt = 100, n = 5, interval = 12, t_inf = 1)
par <- list(KA = 1, CL = 5, V = 50)

## have to be explicit about t_obs with sim_core!
f1 <- function() {
  res <- sim(ode = pk1, regimen = reg, parameters = par, only_obs = TRUE, t_obs=c(0:24))$y
  return(res)
}

f2 <- function() {
  obj <- sim(ode = pk1, regimen = reg, parameters = par, only_obs = TRUE, t_obs=c(0:24), return_design=TRUE)
  sim_core(obj, ode = pk1)$y
}

assert("same results", max(f1() - f2()) < 1e-5)
