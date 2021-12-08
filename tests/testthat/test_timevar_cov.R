test_that("timevarying covariates handled", {
  skip_on_cran()
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
  mod <- new_ode_model(
    code = "
      dAdt[1] = -(Q/V)*A[1] + (Q/V2)*A[2] -(CLi/V)*A[1];
      dAdt[2] = -(Q/V2)*A[2] + (Q/V)*A[1];
    ",
    pk_code = "CLi = CL + CRCL",
    obs = list(cmt = 2, scale = "V"),
    covariates = covs, declare_variables = "CLi"
  )
  par <- list(CL = 3, V = 50, Q = 2.5, V2 = 70)
  reg <- new_regimen(amt = 250, n = 60, interval = 6, type = 'infusion',t_inf = 1)
  t_obs <- seq(0, 360, 0.1)

  sim1 <- sim_ode(
    mod,
    parameters = par,
    regimen = reg,
    covariates = covs,
    only_obs = TRUE,
    t_obs = t_obs,
    output_include = list(parameters = TRUE, covariates = TRUE)
  )

  expect_equal(sim1$CRCL[sim1$t == 209], 6.2)
  expect_equal(sim1$CRCL[sim1$t == 210], 600)
  expect_true(sim1$y[sim1$t == 35 * 6] > 10 * sim1$y[sim1$t == 60 * 6])
})

