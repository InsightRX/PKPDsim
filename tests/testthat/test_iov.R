pars <- list(
  "kappa_CL_1" = 0,
  "kappa_CL_2" = 0,
  "kappa_CL_3" = 0,
  "eta_CL" = 0,
  "CL" = 5,
  "V" = 50,
  "KA" = 1
)

pk1 <- new_ode_model(
  code = "
      CL_iov = CL * exp(kappa_CL + eta_CL);
      dAdt[1] = -KA * A[1]
      dAdt[2] = +KA * A[1] -(CL_iov/V) * A[2]
    ",
  iov = list(
    cv = list(CL = 0.2),
    n_bins = 3
  ),
  obs = list(cmt = 2, scale = "V"),
  dose = list(cmt = 1, bioav = 1),
  declare_variables = c("kappa_CL", "CL_iov"),
  parameters = names(pars),
  cpp_show_code = F
)

reg1 <- new_regimen(
  amt = 100,
  interval = 24,
  n = 5,
  type = "infusion"
)
iov_var <- 0.3 ^ 2 # 30% IOV

test_that("Throws warning when `iov_bins` length doesn't match number of specified bins", {
  expect_warning({
    sim(
      ode = pk1,
      parameters = pars,
      regimen = reg1,
      omega = c(
        iov_var, # IOV in CL
        0, iov_var,
        0, 0, iov_var,
        0, 0, 0, iov_var,
        0, 0, 0, 0, 0.3 # IIV in CL
      ),
      n = 1,
      iov_bins = c(0, 24, 48, 72, 999), # one bin too many
      omega_type = "normal",
      only_obs = TRUE,
      output_include = list(parameters = TRUE, variables = TRUE)
    )
  }, "Number of IOV bins specified")
  Rcpp_v <- unlist(packageVersion("Rcpp"))
  if(Rcpp_v[1] >= 1 && Rcpp_v[2] >= 0 && (Rcpp_v[3] >= 13 || isTRUE(Rcpp_v[3] >= 12 && Rcpp_v[4] >= 4))) {
    ## if-statement can be removed when Rcpp on CRAN >= 1.0.12.4
    expect_warning({
      expect_warning({
        sim(
          ode = pk1,
          parameters = pars,
          regimen = reg1,
          omega = c(
            iov_var, # IOV in CL
            0, iov_var,
            0, 0, iov_var,
            0, 0, 0, iov_var,
            0, 0, 0, 0, 0.3 # IIV in CL
          ),
          n = 1,
          iov_bins = c(0, 24, 999), # one bin too few
          omega_type = "normal",
          only_obs = TRUE,
          output_include = list(parameters = TRUE, variables = TRUE)
        )
      }, "Number of IOV bins specified")
    }, "subscript out of bounds") # only thrown when Rcpp >= 1.0.12.4
  }
})

test_that("IOV is added to parameters", {
  skip_on_cran()
  set.seed(32)

  dat <- sim(
    ode = pk1,
    parameters = pars,
    regimen = reg1,
    omega = c(
      iov_var, # IOV in CL
      0, iov_var,
      0, 0, iov_var,
      0, 0, 0, iov_var,
      0, 0, 0, 0, 0.3 # IIV in CL
    ),
    n = 5,
    iov_bins = c(0, 24, 48, 72),
    omega_type = "normal",
    only_obs = TRUE,
    output_include = list(parameters = TRUE, variables = TRUE)
  )

  expect_equal(
    signif(dat$kappa_CL[dat$t == 12], 4),
    signif(dat$kappa_CL_1[dat$t == 1], 4)
  )
  expect_equal(
    signif(dat$kappa_CL[dat$t == 36], 4),
    signif(dat$kappa_CL_2[dat$t == 1], 4)
  )
  expect_equal(
    signif(dat$kappa_CL[dat$t == 48], 4),
    signif(dat$kappa_CL_3[dat$t == 1], 4)
  )
  expect_equal(
    signif(exp(dat$kappa_CL + dat$eta_CL) * dat$CL, 4),
    signif(dat$CL_iov, 4)
  )
})

test_that("Change in F in 2nd bin is applied in 2nd bin and not later.", {
  # Previously this was an issue because F, when defined in pk_code(), was not updated before the
  # dose was applied to the state vector, so the bioavailability was not applied at the right time.
  # This was fixed by rearranging the order of execution in sim.cpp in the main loop.

  pars <- list(
    "CL" = 5,
    "V" = 50,
    "KA" = 1,
    "F" = 1
  )
  pk1 <- new_ode_model(
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
    parameters = names(pars),
    cpp_show_code = F
  )
  reg <- new_regimen(amt = 800, interval = 24, n = 10, type = "oral")

  # For a first simulation, we're simulating with no variability across the IOV bins:
  pars$kappa_F_1 <- 0
  pars$kappa_F_2 <- 0
  pars$kappa_F_3 <- 0
  args_sim1 <- args <- list(
    ode = pk1,
    parameters = pars,
    regimen = reg,
    only_obs = TRUE,
    t_obs = seq(0, 50, .25),
    iov_bins = c(0L, 24L, 48L, 9999L)
  )
  # For a second simulation, we're applying a change in parameter for the 2nd bin (24-48 hrs).
  # This should affect predictions from 24 onward.
  pars$kappa_F_2 <- 1 # 2nd bin
  args_sim2 <- args <- list(
    ode = pk1,
    parameters = pars,
    regimen = reg,
    only_obs = TRUE,
    t_obs = seq(0, 50, .25),
    iov_bins = c(0L, 24L, 48L, 9999L)
  )
  res1 <- do.call("sim_ode", args = args_sim1)
  res2 <- do.call("sim_ode", args = args_sim2)
  expect_true(min(res1[res1$y != res2$y,]$t) <= 25)
})

test_that("error is not invoked when using parameters_table", {
  parameters_table <- data.frame(
    CL = runif(10), V = runif(10), KA = runif(10), eta_CL = runif(10),
    kappa_CL_1 = 0, kappa_CL_2 = 0, kappa_CL_3 = 0, kappa_CL_4 = 0
  )

  # specifying both parameters_table but for a model with IOV should not fail!
  expect_silent(
    dat <- sim(
      ode = pk1,
      parameters_table = parameters_table,
      regimen = reg1,
      omega = c(
        iov_var, # IOV in CL
        0, iov_var,
        0, 0, iov_var,
        0, 0, 0, iov_var,
        0, 0, 0, 0, 0.3 # IIV in CL
      ),
      n = 5,
      iov_bins = c(0, 24, 48, 72),
      omega_type = "normal",
      only_obs = TRUE,
      output_include = list(parameters = TRUE, variables = TRUE)
    )
  )

  # specifying both parameters and parameters_table should fail
  expect_error(
    dat <- sim(
      ode = pk1,
      parameters = pars,
      parameters_table = parameters_table,
      regimen = reg1,
      omega = c(
        iov_var, # IOV in CL
        0, iov_var,
        0, 0, iov_var,
        0, 0, 0, iov_var,
        0, 0, 0, 0, 0.3 # IIV in CL
      ),
      n = 5,
      iov_bins = c(0, 24, 48, 72),
      omega_type = "normal",
      only_obs = TRUE,
      output_include = list(parameters = TRUE, variables = TRUE)
    )
  )
})
