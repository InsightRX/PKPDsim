# shared parameters

test_that("Effective parameters calculated from covariate expression", {
  # model defined in setup.R, with CLi = CL * pow(WT/70, 0.75)
  covs1 <- list(WT = new_covariate(50))
  pars <- list(KA = 0.5, CL = 5, V = 50)
  eff1 <- calculate_parameters(oral_1cmt_allometric, pars, covs1)
  expect_pars <- attr(oral_1cmt_allometric, "parameters")
  expect_vars <- attr(oral_1cmt_allometric, "variables")

  expect_true(all(c(expect_pars, expect_vars) %in% names(eff1)))
  expect_equal(
    eff1$CLi,
    pars$CL * (covs1$WT$value / 70) ** 0.75
  )
})

test_that("Returned object can be customized.", {
  expect_pars <- attr(oral_1cmt_allometric, "parameters")
  expect_vars <- attr(oral_1cmt_allometric, "variables")

  eff_no_par <- calculate_parameters(
    oral_1cmt_allometric,
    list(KA = 0.5, CL = 5, V = 50),
    list(WT = new_covariate(50)),
    include_parameters = FALSE
  )

  eff_no_var <- calculate_parameters(
    oral_1cmt_allometric,
    list(KA = 0.5, CL = 5, V = 50),
    list(WT = new_covariate(50)),
    include_variables = FALSE
  )

  expect_true(all(expect_vars %in% names(eff_no_par)))
  expect_true(all(expect_pars %in% names(eff_no_var)))
  expect_false(any(expect_pars %in% names(eff_no_par)))
  expect_false(any(expect_vars %in% names(eff_no_var)))
})

test_that("Works if no covariates", {
  # uses a model defined in setup.R
  expect_pars <- attr(mod_1cmt_oral, "parameters")
  expect_vars <- attr(mod_1cmt_oral, "variables")
  pars <- list(KA = 0.5, CL = 5, V = 50)

  eff2 <- calculate_parameters(mod_1cmt_oral, pars, NULL)
  expect_true(all(c(expect_pars, expect_vars) %in% names(eff2)))
  expect_equal(eff2$CL, pars$CL)
})

test_that("Works when t_obs specified for timevarying model", {
  # model defined in setup.R, with CLi = CL * pow(WT/70, 0.75), and x1.5 after 168 hrs
  covs1 <- list(WT = new_covariate(50))
  pars <- list(KA = 0.5, CL = 5, V = 50)
  eff1 <- calculate_parameters(oral_1cmt_allometric, pars, covs1, t_obs = c(20, 40, 200))
  expect_pars <- attr(oral_1cmt_allometric, "parameters")
  expect_vars <- attr(oral_1cmt_allometric, "variables")
  expect_true(all(c(expect_pars, expect_vars) %in% names(eff1)))
  CL1 <- pars$CL * (covs1$WT$value / 70) ** 0.75
  CL2 <- CL1 * 1.5
  expect_equal(
    eff1$CLi,
    c(CL1, CL1, CL2)
  )
})

test_that("Works for dose-dependent model", {
  # model defined in setup.R, with CLi = CL * pow(WT/70, 0.75), and x1.5 after 168 hrs
  covs1 <- list(WT = new_covariate(50))
  pars <- list(KA = 0.5, CL = 5, V = 50)
  reg_low <- new_regimen(amt = 500, times = 0, type = "bolus")
  reg_high <- new_regimen(amt = 1500, times = 0, type = "bolus")
  eff1 <- calculate_parameters(oral_1cmt_allometric, pars, covs1)
  eff2 <- calculate_parameters(oral_1cmt_allometric, pars, covs1, regimen = reg_low)
  eff3 <- calculate_parameters(oral_1cmt_allometric, pars, covs1, regimen = reg_high)
  expect_equal(eff1, eff2)
  expect_equal(eff3$Vi, eff1$Vi * 2)
})
