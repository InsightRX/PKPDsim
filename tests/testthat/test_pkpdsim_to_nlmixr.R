## Uses model defined in setup.R

test_that("nlmixr function is returned", {
  par   <- list(CL = 5, V = 50)
  ruv   <- list(prop = 0.1, add = 1)
  omega <- c(0.1, 0.05, 0.1)
  f <- pkpdsim_to_nlmixr(
    model = mod_1cmt_iv,
    parameters = par,
    omega = c(0.1, 0.05, 0.1),
    res_var = list(prop = 0.1, add = 0.1),
    log_transform = T
  )
  expect_true("function" %in% class(f))
})
