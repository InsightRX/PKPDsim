# Test t_init functionality

## e.g. TDM before first dose:
## at t=-8, conc=10000
## Use this as true value in the simulations
par   <- list(CL = 7.67, V = 97.7, TDM_INIT = 500)
mod <- new_ode_model(
  code = "dAdt[1] = -(CL/V)*A[1]; CONC = A[1]/V",
  state_init = "A[1] = TDM_INIT * V",
  parameters = par,
  obs = list(cmt = 1, scale = "V"),
  declare_variables = c("CONC"),
  cpp_show_code = F
)
reg <- new_regimen(amt = 100000, times=c(0, 24), type="bolus")
s <- sim(
  ode = mod,
  parameters = par,
  n_ind = 1,
  regimen = reg,
  only_obs = TRUE,
  output_include = list(variables = TRUE),
  t_init = 10
)

test_that("TDM before first dose is considered a true initial value", {
  skip_on_cran()
  expect_equal(sum(is.na(s$y)), 0)
  expect_equal(s$y[1], 500)
  expect_equal(round(s$y[3]), 427)
  expect_equal(round(s$y[13]),1157)
})

test_that("Variables are set (also in first row) when TDM before first dose", {
  skip_on_cran()
  expect_equal(round(s$CONC[1:5], 1), c(500, 462.2, 427.3, 395.1, 365.3))
})


