ode_code <- "
    CLi    = CL_avg * exp(kappa_CL) \n;
    Vi     = V_avg * exp(kappa_V) \n;
    dAdt[0] = -(CLi/Vi)*A[0] + (Qi/V2i)*A[1] - (Qi/Vi)*A[0] \n;
    dAdt[1] =                - (Qi/V2i)*A[1] + (Qi/Vi)*A[0] \n;
    dAdt[2] =  (A[0]/scale)
  "
pk_code <- "
    V2i    = V2_avg * exp(kappa_V2) \n;
  "

test_that("checks code for iov terms", {
  iov1 <- list(n_bins = 1, cv = list(CL = 0.1, V = 0.1))
  iov2 <- list(n_bins = 1, cv = list(CL = 0.1, V = 0.1, V2 = 0.1))
  iov3 <- list(n_bins = 1, cv = list(CL = 0.1, V = 0.1, V2 = 0.1, Q = 0.1))
  expect_null(check_iov_specification(iov1, ode_code, pk_code))
  expect_null(check_iov_specification(iov2, ode_code, pk_code))
  expect_message(
    check_iov_specification(iov2, ode_code, " "),
    "IOV requested for parameter V2 but no kappa_V2 found"
  )
  expect_message(
    check_iov_specification(iov3, ode_code, pk_code),
    "IOV requested for parameter Q but no kappa_Q found"
  )
})

test_that("IOV checks stop if key IOV fields missing", {
  iov_empty <- list()
  iov_cv_wrong <- list(n_bins = 2, cv = 0.17)
  iov_bins_missing <- list(cv = list(CL = 0.17))
  expect_error(
    check_iov_specification(iov_empty, ode_code, pk_code),
    "IOV misspecified."
  )
  expect_error(
    check_iov_specification(iov_cv_wrong, ode_code, pk_code),
    "IOV misspecified."
  )
  expect_error(
    check_iov_specification(iov_bins_missing, ode_code, pk_code),
    "IOV misspecified."
  )
})
