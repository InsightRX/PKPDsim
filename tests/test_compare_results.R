# Test: numeric accuracy
# compare with analytic equations

library(PKPDsim)
library(testit) ## testthat doesn't play nice with sourceCpp

Sys.setenv("R_TESTS" = "")

## parameters:
p <- list(KA = 1, CL = 5, V = 50)
t_obs <- c(0:72)
dose <- 100
t_dose <- c(0)
regimen <- new_regimen(amt=dose, times = t_dose)

## models:
pk1cmt_oral_anal = function(t, dose, KA, V, CL) {
  dose*KA/(V*(KA-CL/V))*(exp(-(CL/V) * t)-exp(-KA * t))
}
pk1cmt_oral_lib  <- new_ode_model("pk_1cmt_oral")
pk1cmt_oral_code <- new_ode_model(code = "
                                  dAdt[1] = -KA*A[1];
                                  dAdt[2] = KA*A[1] - (CL/V)*A[2];", obs=list(cmt = 2, scale="V")
)

res <- list()
res$pk1cmt_oral_lib  <- sim_ode(ode=pk1cmt_oral_lib, par=p, regimen=regimen, t_obs=t_obs, only_obs=TRUE)
res$pk1cmt_oral_code <- sim_ode(ode=pk1cmt_oral_code, par=p, regimen=regimen, t_obs=t_obs, only_obs=TRUE)
res$pk1cmt_oral_anal <- pk1cmt_oral_anal(t_obs, dose, p$KA, p$V, p$CL)
assert('library C++ ODE equals analytical function',
       all.equal(round(res$pk1cmt_oral_lib$y, 4), round(res$pk1cmt_oral_anal, 4)))
assert('custom C++ ODE equals analytical function',
       all.equal(round(res$pk1cmt_oral_code$y, 4), round(res$pk1cmt_oral_anal, 4)))
