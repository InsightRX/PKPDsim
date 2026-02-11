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

test_that("IOV: Change in F in 2nd bin is applied in 2nd bin and not later.", {
  skip_on_cran()
  # Previously this was an issue because F, when defined in pk_code(), was not updated before the
  # dose was applied to the state vector, so the bioavailability was not applied at the right time.
  # This was fixed by rearranging the order of execution in sim.cpp in the main loop.

  pars_iov_f <- list(
    "CL" = 5,
    "V" = 50,
    "KA" = 1,
    "F" = 1
  )
  pk_iov_f <- new_ode_model(
    code = "
        dAdt[1] = -KA * A[1]
        dAdt[2] = +KA * A[1] -(CLi/V) * A[2]
    ",
    pk_code = "
        Fi = F * exp(kappa_F);
        CLi = CL;
    ",
    iov = list(
      cv = list(F = 0.2),
      n_bins = 3
    ),
    obs = list(cmt = 2, scale = "V"),
    dose = list(cmt = 1, bioav = "Fi"),
    declare_variables = c("kappa_F", "Fi", "CLi"),
    parameters = names(pars_iov_f),
    cpp_show_code = FALSE
  )
  reg <- new_regimen(amt = 800, interval = 24, n = 10, type = "oral")

  # For a first simulation, we're simulating with no variability across the IOV bins:
  pars_iov_f$kappa_F_1 <- 0
  pars_iov_f$kappa_F_2 <- 0
  pars_iov_f$kappa_F_3 <- 0
  args_sim1 <- args <- list(
    ode = pk_iov_f,
    parameters = pars_iov_f,
    regimen = reg,
    only_obs = TRUE,
    t_obs = seq(0, 50, .25),
    iov_bins = c(0L, 24L, 48L, 9999L)
  )
  # For a second simulation, we're applying a change in parameter for the 2nd bin (24-48 hrs).
  # This should affect predictions from 24 onward.
  pars_iov_f$kappa_F_2 <- 1 # 2nd bin
  args_sim2 <- args <- list(
    ode = pk_iov_f,
    parameters = pars_iov_f,
    regimen = reg,
    only_obs = TRUE,
    t_obs = seq(0, 50, .25),
    iov_bins = c(0L, 24L, 48L, 9999L)
  )
  res1 <- do.call("sim_ode", args = args_sim1)
  res2 <- do.call("sim_ode", args = args_sim2)
  expect_true(min(res1[res1$y != res2$y,]$t) <= 25)
})

describe("Models with bioavailability", {
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
})