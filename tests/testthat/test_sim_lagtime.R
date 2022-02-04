test_that("dose dump after lagtime in correct order in output data", {
  skip_on_cran()
  reg <- new_regimen(amt = 500, n = 4, interval = 12, type = 'oral')
  pars <- list(CL = 5, V = 50, KA = 0.5, TLAG = 0.83)
  mod <- new_ode_model(
    code = "
      dAdt[0] = -KA * A[0]
      dAdt[1] = +KA * A[0] -(CL/V) * A[1]
  ",
    lagtime = c("TLAG", 0),
    obs = list(cmt = 2, scale = "V"),
    dose = list(cmt = 1, bioav = 1),
    parameters = pars
  )
  dat <- sim_ode(
    ode = mod,
    regimen = reg,
    parameters = pars,
    only_obs = FALSE
  )
  expect_equal(round(dat[dat$t == 12.83 & dat$comp == 1,]$y, 1), c(1.2, 501.2))
})
