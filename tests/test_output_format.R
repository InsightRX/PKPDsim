# Test: output dataset correctly formatted and correct number of datapoints

library(PKPDsim)

## parameters
t_obs   <- c(0:72)
t_dose  <- c(0,12,24)
dose    <- 100

p       <- list(KA=1.8, V=30, CL=1.7)
regimen <- new_regimen(amt=dose, times=t_dose, type="oral")
res     <- list()

test_that("sim output correctly formatted", {
  pk1cmt_oral_code <- new_ode_model(code = "
                                    dAdt[1] = -KA*A[1];
                                    dAdt[2] = KA*A[1] - (CL/V)*A[2];", obs=list(cmt = 2, scale="V")
  )
  # all compartments
  res$pk1cmt_oral_all       <- sim_ode(ode=pk1cmt_oral_code, par=p, regimen=regimen, t_obs=t_obs)
  # only obs, but without filterting dublicate t_obs
  res$pk1cmt_oral_obs       <- sim_ode(ode=pk1cmt_oral_code, par=p, regimen=regimen, t_obs=t_obs, only_obs=TRUE, duplicate_t_obs = TRUE)
  # only obs, with filterting dublicate t_obs
  res$pk1cmt_oral_obs_nodup <- sim_ode(ode=pk1cmt_oral_code, par=p, regimen=regimen, t_obs=t_obs, only_obs=TRUE, duplicate_t_obs = FALSE)

  expect_equal(length(res$pk1cmt_oral_all[1,]), 4)
  expect_equal(length(res$pk1cmt_oral_code[1,]), 4)
  expect_equal(length(res$pk1cmt_oral_code_obs[1,]), 4)
  expect_equal(unique(length(unique(res$pk1cmt_oral_all$comp))), 3)
  expect_equal(unique(length(res$pk1cmt_oral_obs$t)), length(t_obs) + length(t_dose)-1)
  expect_equal(unique(length(res$pk1cmt_oral_obs_nodup$y)), length(t_obs))
}
