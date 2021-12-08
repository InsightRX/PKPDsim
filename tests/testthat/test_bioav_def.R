parameters <- list(KA = 0.5, CL = 5, V = 50)
covs <- list(WT = new_covariate(50))
reg <- new_regimen(amt = 100, n = 1, interval = 12, type = "bolus")
mod1 <- oral_1cmt_allometric # defined in setup.R using same covs and pars as above
y1 <- sim_ode(mod1, parameters = parameters, regimen = reg, covariates = covs, only_obs=TRUE)$y

test_that("bioav numeric option working", {
  skip_on_cran()
  mod2 <- new_ode_model(
    code = "
    CLi = CL * pow(WT/70, 0.75)
    dAdt[1] = -KA * A[1]
    dAdt[2] = KA*A[1] - (CLi/V)*A[2]
  ",
  dose = list(cmt = 1, bioav = 0.5),
  covariates = covs,
  declare_variables = "CLi",
  parameters = parameters
  )

  y2 <- sim_ode(mod2, parameters = parameters, regimen = reg, covariates = covs, only_obs=TRUE)$y

  expect_equal(round(y1,1),  round(y2*2, 1))
})

test_that("bioav string option working", {
  skip_on_cran()
  mod3 <- new_ode_model(
    code = "
    CLi = CL * pow(WT/70, 0.75)
    dAdt[1] = -KA * A[1]
    dAdt[2] = KA*A[1] - (CLi/V)*A[2]
  ",
  dose = list(cmt = 1, bioav = "WT/70"),
  covariates = covs,
  declare_variables = "CLi",
  parameters = parameters
  )

  y3 <- sim_ode(mod3, parameters = parameters, regimen = reg, covariates = covs, only_obs=TRUE)$y

  expect_equal(round(y1*(50/70),1),  round(y3, 1))
})

