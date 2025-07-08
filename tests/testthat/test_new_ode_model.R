test_that("Double absorption models work appropriately", {
  skip_on_cran()
  mod <- new_ode_model(
    code = "dAdt[1] = -KA1*A[1]; dAdt[2] = -KA2*A[2]; dAdt[3] = KA1*A[1] + KA2*A[2] -(CL/V)*A[3]; ",
    pk_code = NULL,
    parameters = c("CL", "V", "KA1", "KA2"),
    obs = list(cmt = 1, scale = 1),
    dose = list(
      cmt = 1,
      duplicate = c(2),
      bioav = c(0.3, 0.6, 0.9)
    ),
    lagtime = c(0, 1.5),
    cpp = F
  )
  reg <- new_regimen(amt = 100, n = 12, interval = 12, type = "oral")
  res <- sim(
    mod,
    regimen = reg,
    parameters = list(
      KA1 = 1,
      KA2 = 1,
      CL = 1,
      V = 10
    ),
    only_obs = F,
    output_include = list(variables=TRUE),
    t_obs = seq(0, 24, .1)
  )
  expect_equal(attr(mod, "dose")$duplicate, 2)
  expect_equal(round(res[res$comp == 1 & res$t == 0.1, ]$y, 2), 27.15) # abs comp 1
  expect_equal(res[res$comp == 2 & res$t == 1.2, ]$y, 0) # abs comp 2: should still be 0 due to lagtime
  expect_equal(round(res[res$comp == 2 & res$t == 1.6, ]$y, 1), 54.3) # abs comp 2: should be twice the amount of t=0.1&comp==1
})

test_that("All IOV bins are properly activated", {
  iov_bins <- c(0, 12, 24, 36, 48, 9999)
  mod <- new_ode_model(
    code = "CLi = CL * exp(kappa_CL); dAdt[1] = -(CLi/V)*A[1]; ",
    pk_code = NULL,
    parameters = c("CL", "V", "kappa_CL_1", "kappa_CL_2", "kappa_CL_3", "kappa_CL_4", "kappa_CL_5"),
    declare_variables = c("CLi", "kappa_CL"),
    obs = list(cmt = 1, scale = 1),
    dose = list(cmt = 1, bioav = 1),
    cpp = FALSE,
    iov = list(
      use = TRUE,
      cv = list(CL = 0.2),
      n_bins = 5,
      bins = c(0, 12, 24, 36, 48, 9999)
    )
  )
  reg <- new_regimen(amt = 100, n = 12, interval = 12, t_inf = 1, type = "infusion")
  res <- sim(
    mod,
    regimen = reg,
    parameters = list(
      CL = 1,
      V = 10,
      kappa_CL_1 =  0.1,
      kappa_CL_2 =  0.2,
      kappa_CL_3 =  0.3,
      kappa_CL_4 =  0.4,
      kappa_CL_5 = -0.5
    ),
    iov_bins = iov_bins,
    only_obs = TRUE,
    output_include = list(variables=TRUE),
    t_obs = iov_bins[1:5] + 1
  )

  expect_equal(round(res$kappa_CL, 2), c(0.10, 0.20, 0.30, 0.40, -0.50))
})

test_that("correct combinations of vars/scale/cmt are supported", {
  skip_on_cran() # slow

  ## cmt and scale same lengths: ok
  expect_error(
    new_ode_model(
      code = "
      dAdt[1] = -KA * A[1];
      dAdt[2] = -(CL/V) * A[2] + KA*A[1];
    ",
    obs = list(
      cmt = c(1, 2),
      scale = c(1, "V"),
      label = c("abs", "conc")
    ),
    cpp_show_code = FALSE
    ),
    NA
  )

  ## cmt length > 1 and scale length = 1: ok
  expect_error(
    new_ode_model(
      code = "
      dAdt[1] = -KA * A[1];
      dAdt[2] = -(CL/V) * A[2] + KA*A[1];
    ",
    obs = list(
      cmt = c(1, 2),
      scale = c(1),
      label = c("abs", "conc")
    ),
    cpp_show_code = FALSE
    ),
    NA
  )

  ## cmt and scale different lengths: error
  expect_error(
    new_ode_model(
      code = "dAdt[1] = -(CL/V)*A[1]; CONC = 1000*A[1]/V; METAB = CONC/2; METAB2 = CONC * t; ACT = 15",
      obs = list(cmt = 1, scale = c(1, 0.5)),
      cpp_show_code = FALSE
    )
  )

  ## both variable and scale are provided: error
  expect_error(
    new_ode_model(
      code = "dAdt[1] = -(CL/V)*A[1]; CONC = 1000*A[1]/V; METAB = CONC/2; METAB2 = CONC * t; ACT = 15",
      obs = list(
        variable = c("CONC", "METAB", "METAB2", "ACT"),
        scale = 1
      ),
      declare_variables = vars,
      cpp_show_code = FALSE
    )
  )

})

test_that("adding development info works", {
  skip_on_cran()
  dev <- list(
    n_patients = 50,
    n_sites = 1,
    n_tdms = 200,
    age = list(min = 15, median = 50, max = 90, unit = "years")
  )
  mod_1cmt_tmp <- new_ode_model(
    code = "
      dAdt[1] = -KA * A[1];
      dAdt[2] = -(CL/V) * A[2] + KA*A[1];
    ",
    development = dev
  )
  expect_identical(
    attr(mod_1cmt_tmp, "development"),
    dev
  )
})

test_that("specifying overlapping covariates and variables throws error", {
  expect_error({
    tmp <- new_ode_model(
      code = "dAdt[1] = -(CL*WT/V)*A[1]; CONC = 1000*A[1]/V; METAB = CONC/2; METAB2 = CONC * t; ACT = 15",
      covariates = list(WT = new_covariate(70)),
      declare_variables = c("WT")
    )
  })
})
