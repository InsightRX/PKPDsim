# Test: numeric accuracy
# compare with analytic equations

library(PKPDsim)

## parameters
t_obs   <- c(0:24)
dose    <- 100
p       <- list(KA=1.8, V=30, CL=1.7)
regimen <- new_regimen(amt=dose, times=c(0))

test_that("sim equals analytic eq for pk1cmt oral single dose", {
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
  expect_equal(round(res$pk1cmt_oral_lib$y, 4), round(res$pk1cmt_oral_anal, 4))
  expect_equal(round(res$pk1cmt_oral_code$y, 4), round(res$pk1cmt_oral_anal, 4))
})

