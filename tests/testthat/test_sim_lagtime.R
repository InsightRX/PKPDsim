test_that("dose dump after lagtime in correct order in output data", {
  skip_on_cran()
  reg <- new_regimen(amt = 500, n = 4, interval = 12, type = 'oral')
  pars <- list(CL = 5, V = 50, KA = 0.5, TLAG = 0.83)
  dat <- sim_ode(
    ode = mod_1cmt_oral_lagtime,
    regimen = reg,
    parameters = pars,
    only_obs = FALSE
  )
  ## Change after RXR-2394: time of TLAG not in dataset anymore unless requested by user in t_obs
  ## Before change: expect_equal(round(dat[dat$t == 12.83 & dat$comp == 1,]$y, 1), c(1.2, 501.2))
  ## After change:
  expect_equal(nrow(dat[dat$t == 12.83,]), 0)
  ## When grid requested by user, lagtime should be visible
  dat <- sim_ode(
    ode = mod_1cmt_oral_lagtime,
    regimen = reg,
    parameters = pars,
    t_obs = seq(0, 1, 0.01),
    only_obs = TRUE
  )
  tmp <- dat[dat$t >= 0.82 & dat$t <= 0.85, ]
  expect_equal(tmp$t, c(0.82, 0.83, 0.84, 0.85))
  expect_equal(round(tmp$y, 2), c(0, 0, 0.05, 0.10))
})
