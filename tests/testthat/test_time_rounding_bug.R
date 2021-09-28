# rounding time should not produce NAs in sim
## time-rounding bug 20170804

test_that("No NAs related to rounding", {
  # Uses models defined in setup.R
  p <- list(CL = 5, V = 50, Q = 10, V2 = 150)
  r1 <- new_regimen(amt = 100, times = c(0, 24, 36), type = "infusion")
  dat1 <- sim_ode(
    ode = mod_2cmt_iv,
    parameters = p,
    regimen = r1,
    t_obs=seq(0, 150, length.out = 100)
  )
  expect_equal(sum(is.na(dat1$y)), 0)
})
