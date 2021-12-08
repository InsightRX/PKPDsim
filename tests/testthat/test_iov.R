test_that("IOV is added to parameters", {
  skip_on_cran()
  set.seed(32)
  pars <- list(
    "kappa_CL_1" = 0,
    "kappa_CL_2" = 0,
    "kappa_CL_3" = 0,
    "kappa_CL_4" = 0,
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
      n_bins = 4
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

