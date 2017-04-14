library(PKPDsim)
library(testit)
Sys.setenv("R_TESTS" = "")

pk1 <- new_ode_model(code = "
  dAdt[1] = -KA * A[1]
  dAdt[2] = +KA * A[1] -(CL/V) * A[2]
", obs = list(cmt = 2) )

reg <- new_regimen(amt = 100, n = 5, interval = 12, t_inf = 1)
par <- list(KA = 1, CL = 5, V = 50)

f1 <- function() {
  res <- sim(ode = pk1, regimen = reg, parameters = par, only_obs = TRUE)
  obs <- res$y
  return(obs)
}

f2 <- function() {
  obj <- sim(ode = pk1, regimen = reg, parameters = par, only_obs = TRUE, return_design=TRUE)
  sim_core(obj, ode = pk1)
}

assert("same results", max(f1() - f2()) < 1e-5)
