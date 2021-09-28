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

