# Uses model defined in setup.R (conditional, NOT_CRAN only):
# - mod_2cmt_timevar

skip_on_cran()

par_timevar <- list(CL = 3, V = 50, Q = 2.5, V2 = 70)
reg_timevar <- new_regimen(
  amt = 250,
  n = 60,
  interval = 6,
  type = 'infusion',
  t_inf = 1
)

test_that("timevarying covariates handled", {
  # CLi changes by several orders of magnitude after
  # steady state is achieved, which should produce
  # a new steady state that is much lower
  covs <- list(
    CRCL = new_covariate(
      value = c(4.662744, 5.798767, 6.195943, 6.2, 600),
      times = c(0, 18, 29, 209, 210),
      implementation = "locf"
    )
  )
  t_obs <- seq(0, 360, 0.1)
  sim1 <- sim_ode(
    mod_2cmt_timevar,
    parameters = par_timevar,
    regimen = reg_timevar,
    covariates = covs,
    only_obs = TRUE,
    t_obs = t_obs,
    output_include = list(parameters = TRUE, covariates = TRUE)
  )
  expect_equal(sim1$CRCL[sim1$t == 209], 6.2)
  expect_equal(sim1$CRCL[sim1$t == 210], 600)
  expect_true(sim1$y[sim1$t == 35 * 6] > 10 * sim1$y[sim1$t == 60 * 6])
})

test_that("timevarying covariates are interpolated and affect PK", {
  covs_locf <- list(
    CRCL = new_covariate(
      times = c(0, 48, 96),
      value = c(3, 10, 3),
      implementation = "locf"
    )
  )
  covs_inter <- list(
    CRCL = new_covariate(
      times = c(0, 48, 96),
      value = c(3, 10, 3),
      implementation = "interpolate"
    )
  )
  t_obs <- seq(0, 120, 2)
  sim2_inter <- sim_ode(
    mod_2cmt_timevar,
    parameters = par_timevar,
    regimen = reg_timevar,
    covariates = covs_inter,
    only_obs = TRUE,
    t_obs = t_obs,
    output_include = list(parameters = TRUE, covariates = TRUE, variables = TRUE)
  )
  sim2_locf <- sim_ode(
    mod_2cmt_timevar,
    parameters = par_timevar,
    regimen = reg_timevar,
    covariates = covs_locf,
    only_obs = TRUE,
    t_obs = t_obs,
    output_include = list(parameters = TRUE, covariates = TRUE, variables = TRUE)
  )

  ## Check covariate changing for inter, but not for locf
  expect_equal(
    round(sim2_inter$CRCL, 3)[1:10],
    c(3, 3.292, 3.583, 3.875, 4.167, 4.458, 4.75, 5.042, 5.333, 5.625)
  )
  expect_equal(
    round(sim2_locf$CRCL, 3)[1:10],
    rep(3, 10)
  )

  ## Check interpolated covariates actually affect PK parameters
  expect_equal(
    round(sim2_inter$CLi, 3)[1:10],
    c(6, 6.292, 6.583, 6.875, 7.167, 7.458, 7.75, 8.042, 8.333, 8.625)
  )
  expect_equal(
    round(sim2_locf$CLi, 3)[1:10],
    rep(6, 10)
  )

  ## Check interpolated covariates actually affect simulated conc
  expect_equal(
    round(sim2_inter$y, 3)[1:10],
    c(0, 0.32, 0.611, 0.788, 1.205, 1.531, 1.708, 2.098, 2.379, 2.503)
  )
  expect_equal(
    round(sim2_locf$y, 3)[1:10],
    c(0, 0.321, 0.617, 0.805, 1.239, 1.598, 1.813, 2.251, 2.598, 2.792)
  )

  ## Visual check:
  # ggplot(sim2_inter, aes(x = t, y = y)) +
  #   geom_line() +
  #   geom_line(data = sim2_locf, colour = "blue")

})
