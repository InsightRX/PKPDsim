
## Set up simulations to test variance:
## Uses model defined in setup.R
reg <- new_regimen(
  amt = 100,
  n = 3,
  interval = 12,
  type = "infusion",
  t_inf = 2
)

par <- list(CL = 5, V = 50)
omega <- c(0.1, 0.0, 0.1)
t_obs <- c(2, 48)
res <- sim_ode(
  mod_1cmt_iv,
  parameters = par,
  regimen = reg,
  t_obs = t_obs,
  only_obs=TRUE
)

## 1 compartment model
test_that("delta approximation and full simulation match", {
  skip_on_cran()
  v_delta <- get_var_y(
    model = mod_1cmt_iv,
    parameters = par,
    t_obs = t_obs,
    regimen = reg,
    omega = omega,
    method = "delta"
  )
  v_sim <- get_var_y(
    model = mod_1cmt_iv,
    parameters = par,
    t_obs = t_obs,
    auc = FALSE,
    regimen = reg,
    omega = omega,
    method="sim",
    n_ind = 2500
  )
  expect_true(max(abs(v_delta$regular - v_sim$regular)/v_sim$regular) <0.1)
  expect_true(max(abs(v_delta$log - v_sim$log)/v_sim$log) < 0.15)
})

test_that("Confidence interval instead of SD", {
  skip_on_cran()
  CI_range <- c(0.1, 0.9)
  reg2 <- new_regimen(
    amt = 2000,
    n = 3,
    interval = 12,
    type = "infusion",
    t_inf = 2
  )
  t_obs2 <- c(2, 12)
  v1_delta <- get_var_y(
      model = mod_1cmt_iv,
      parameters = par,
      t_obs = t_obs2,
      regimen = reg2,
      omega = omega,
      method = "delta",
      q = CI_range
    )
  v2_sim <- get_var_y(
    model = mod_1cmt_iv,
    parameters = par,
    t_obs = t_obs2,
    regimen = reg2,
    omega = omega,
    method ="sim",
    n_ind = 2500,
    q = CI_range
  )
  expect_equal(dim(v1_delta$regular), c(2, 2))
  expect_equal(dim(v2_sim$regular), c(2, 2))
  expect_equal(dim(v1_delta$log), c(2, 2))
  expect_equal(dim(v2_sim$log), c(2, 2))
  # Ensure correct output format
  expect_true(all(v1_delta$regular[1, ] < v1_delta$regular[2, ]))
  expect_true(all(v2_sim$regular[1, ] < v2_sim$regular[2, ]))

  v1_delta_v <- as.vector(v1_delta$regular)
  v2_sim_v <- as.vector(v2_sim$regular)
  expect_true(max(abs(v1_delta_v - v2_sim_v )/v2_sim_v) < 0.1)
})


test_that("Two compartment model", {
  skip_on_cran()
  set.seed(80)
  # Uses models defined in setup.R
  par2 <- list(CL = 1, V = 10, Q = 1, V2 = 10)
  omega2 <- c(0.118,
              0.05, 0.143,
              0.01, 0.01, .29,
              0.01, 0.01, 0.07, .102)
  res <- sim_ode(
    mod_2cmt_iv,
    parameters = par2,
    t_obs = t_obs,
    regimen = reg,
    only_obs=TRUE
  )

  v1 <- get_var_y(
    model = mod_2cmt_iv,
    parameters = par2,
    t_obs = t_obs,
    regimen = reg,
    mega = omega2
  )
  v2 <- get_var_y(
    model = mod_2cmt_iv,
    parameters = par2,
    t_obs = t_obs,
    regimen = reg,
    omega = omega2,
    method = "sim",
    n_ind = 2000
  )

  expect_true(all(abs(((v1$regular - v2$regular)/v2$regular)) < 0.2))
  expect_equal(round(v1$regular, 3), c(2.282, 1.028))

})

test_that("One compartment with MM kinetics", {
  skip_on_cran()
  mod3 <- new_ode_model("pk_1cmt_iv_mm")
  par3 <- list(VMAX = 5, KM = 5, V = 10)
  omega3 <- c(0.1,
              0.05, 0.1,
              0.01, 0.01, 0.1)
  res <- sim_ode(
    mod3,
    parameters = par3,
    t_obs = t_obs,
    regimen = reg,
    only_obs = TRUE
  )

  v1 <- get_var_y(
    model = mod3,
    parameters = par3,
    t_obs = t_obs,
    regimen = reg,
    mega = omega3
  )
  v2 <- get_var_y(
    model = mod3,
    parameters = par3,
    t_obs = t_obs,
    regimen = reg,
    omega = omega3,
    method = "sim",
    n_ind = 2000
  )

  expect_true(all(abs(v1$regular - v2$regular)/res$y < 0.5))
  expect_equal(round(v1$regular, 3), c(0.120, 4.206))

})
