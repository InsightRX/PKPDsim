# Test t_init functionality

## e.g. TDM before first dose:
## at t=-8, conc=10000
## Use this as true value in the simulations

test_that("TDM before first dose is considered a true initial value", {
  skip_on_cran()
  par   <- list(CL = 7.67, V = 97.7, TDM_INIT = 500)
  mod <- new_ode_model(
    code = "dAdt[1] = -(CL/V)*A[1];",
    state_init = "A[1] = TDM_INIT * V",
    parameters = par,
    obs = list(cmt = 1, scale = "V"),
    cpp_show_code=F
  )
  fits <- c()
  omega <- c(0.0406, 0.0623, 0.117)
  reg <- new_regimen(amt = 100000, times=c(0, 24), type="bolus")

  s <- sim(
    ode = mod,
    parameters = par,
    n_ind = 1,
    regimen = reg,
    only_obs = TRUE,
    t_init = 10
  )

  expect_equal(sum(is.na(s$y)), 0)
  expect_equal(s$y[1], 500)
  expect_equal(round(s$y[3]), 427)
  expect_equal(round(s$y[13]),1157)

})

