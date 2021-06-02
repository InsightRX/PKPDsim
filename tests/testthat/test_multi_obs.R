context("Example multiple observation types (e.g. different compartments! not just different residual errors)")

## define parameters
vars <- c("CONC", "METAB", "METAB2")
pk1 <- new_ode_model(
  code = "dAdt[1] = -(CL/V)*A[1]; CONC = 1000*A[1]/V; METAB = CONC/2; METAB2 = CONC * t;",
  obs = list(variable = vars, scale = 1),
  declare_variables = vars,
  cpp_show_code = F
)

regimen  <- new_regimen(amt = 100, interval = 12, n = 5, type="infusion", t_inf = 1)
parameters   <- list("CL" = 15, "V" = 150)
omega <- PKPDsim::cv_to_omega(list("CL" = 0.2, "V" = 0.2), parameters[1:2])

test_that("obs types are output by `sim`", {
  obs_type <- c(1,2,1,3,1)
  data <- sim(
    ode = pk1,
    parameters = list(CL = 20, V = 200),
    regimen = regimen,
    int_step_size = 0.1,
    only_obs = TRUE,
    obs_type = obs_type,
    t_obs = c(2, 4, 6, 8, 12),
    output_include = list("variables" = TRUE)
  )
  expect_equal(data$obs_type, obs_type)
  expect_equal(data$y, diag(as.matrix(data[1:5,5+obs_type])))
})


test_that("check obs at same timepoint but with different obs_type", {
  t_same <- sim(
    ode = pk1,
    parameters = list(CL = 20, V = 200),
    regimen = regimen,
    int_step_size = 0.1,
    only_obs = TRUE,
    obs_type = obs_type,
    t_obs = c(2, 4, 4, 8, 8),
    output_include = list("variables" = TRUE)
  )
  expect_equal(t_same$t[2], t_same$t[3])
  expect_equal(t_same$t[4], t_same$t[5])
  expect_equal(t_same$y[3], t_same$METAB[3])
  expect_equal(t_same$y[2], t_same$CONC[2])
  expect_equal(t_same$y[5], t_same$METAB2[5])
  expect_equal(t_same$y[4], t_same$CONC[4])
})


test_that("check that residual error correctly applied to right var", {
  set.seed(12345)
  ruv_term3 <- list(prop = c(0, 0, 0.1), add = c(0, 0, 0.1))
  ruv_term1 <- list(prop = c(.1, 0, 0), add = c(1, 0, 0))

  data2 <- sim(
    ode = pk1,
    parameters = list(CL = 20, V = 200),
    regimen = regimen,
    int_step_size = 0.1,
    only_obs = TRUE,
    obs_type = obs_type,
    t_obs = c(2, 4, 6, 8, 12),
    output_include = list("variables" = TRUE),
    res_var = ruv_term3
  )
  y <- diag(as.matrix(data2[1:5, 5 + obs_type]))

  # no residual error for obs_type 1 and 2, only 3
  expect_equal(data2$y[-4], y[-4])
  expect_true(data2$y[-4][4] != y[4])

  data3 <- sim(
    ode = pk1,
    parameters = list(CL = 20, V = 200),
    regimen = regimen,
    int_step_size = 0.1,
    only_obs = TRUE,
    obs_type = obs_type,
    t_obs = c(2, 4, 6, 8, 12),
    output_include = list("variables" = TRUE),
    res_var = ruv_term1
  )
  y <- diag(as.matrix(data2[1:5, 5 + obs_type]))

  # no residual error for obs_type 2/3, only and 1
  expect_equal(data3$y[-c(1,3,5)], y[-c(1,3,5)])
  expect_true(all(data3$y[c(1,3,5)] != y[c(1,3,5)]))
})
