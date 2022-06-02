# Example multiple observation types (e.g. different compartments! not just different residual errors)

## define parameters
vars <- c("CONC", "METAB", "METAB2", "ACT")
pk1 <- new_ode_model(
  code = "dAdt[1] = -(CL/V)*A[1]; CONC = 1000*A[1]/V; METAB = CONC/2; METAB2 = CONC * t; ACT = 15",
  obs = list(variable = vars),
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
  obs_type <- c(1,2,1,3,1)
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
  obs_type <- c(1,2,1,3,1)
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


test_that("specifying ruv as multi-type when only 1 obs_type", {
  obs_type <- c(1,2,1,3,1)
  ruv_single <- list(prop = 0.1, add = 1)
  ruv_multi <- list(prop = c(0.1, 1), add = c(1, 20))

  s_single <- sim(
    ode = pk1,
    parameters = parameters,
    n_ind = 1,
    regimen = regimen,
    only_obs = TRUE,
    t_obs = c(2,4,6,8),
    seed = 123456,
    res_var = ruv_single
  )

  ## specified as multi, but obs_type is all 1, so should
  ## give same results as s_single
  s_multi1 <- sim(
    ode = pk1,
    parameters = parameters,
    n_ind = 1,
    regimen = regimen,
    only_obs = TRUE,
    t_obs = c(2,4,6,8),
    obs_type = c(1, 1, 1, 1),
    seed = 123456,
    res_var = ruv_multi,
    t_init = 10
  )
  expect_equal(s_multi1$y, s_single$y)

  ## specifying ruv as multi-type when multiple obs_type
  ## should now give different results
  s_multi1 <- sim(
    ode = pk1,
    parameters = parameters,
    n_ind = 1,
    regimen = regimen,
    only_obs = TRUE,
    t_obs = c(2,4,6,8),
    obs_type = c(1, 2, 1, 2),
    seed = 123456,
    res_var = ruv_multi,
    t_init = 10
  )
  expect_equal(s_multi1$y[c(1, 3)], s_single$y[c(1,3)])
  expect_true(sum(abs(s_single$y[c(2,4)] - s_multi1$y[c(2,4)])) > 50)
})

test_that("multi-obs with baseline and obs_time = dose_time works correctly", {
  tmp <- sim(
    pk1,
    parameters = parameters,
    regimen = regimen,
    t_obs = c(0, 0, 6, 6),
    obs_type = c(1, 4, 1, 4),
    only_obs = TRUE,
    output_include = list(variables = TRUE),
    return_design = F
  )
  expect_equal(
    tmp$y[tmp$obs_type == 1],
    tmp$CONC[tmp$obs_type == 1]
  )
  expect_equal(
    tmp$y[tmp$obs_type == 4],
    tmp$ACT[tmp$obs_type == 4]
  )
})

test_that("multi-obs model simulates correctly (bug 202205)", {
  skip_on_cran()

  pars <- list(
    CL = 23.9,
    CLM = 5.19,
    V = 107,
    Q = 3.31,
    V2 = 46.2,
    VM = 111,
    KA = 18.6,
    TLAG = 0.415
  )
  covs <- list(WT = PKPDsim::new_covariate(80))
  mod <- new_ode_model(
    code = "dAdt[0] = -KAi*A[0] \
    dAdt[1] = +KAi*A[0] - (CLi/Vi)*A[1] - (Qi/Vi)*A[1] + (Qi/V2i)*A[2] \
    dAdt[2] = +(Qi/Vi)*A[1] - (Qi/V2i)*A[2] \
    dAdt[3] = +(CLi/Vi)*A[1] - (CLMi/VMi)*A[3] \
    dAdt[4] = A[1]*1000.0/Vi \
    CONC = A[1]/(Vi/1000.0) \
    CONCM = A[3]/(VMi/1000.0) \
    CONCT = CONC+CONCM \
  ",
    pk = "KAi = KA \
    CLi = CL * pow(WT/70.0, 0.75) \
    Vi = V *(WT/70.0) \
    Qi = Q * pow(WT/70.0, 0.75)\
    V2i = V2 * (WT/70.0) \
    CLMi = CLM * pow(WT/70.0, 0.75) \
    VMi = VM *(WT/70.0) \
  ",
    parameters = pars,
    declare_variables = c( "CLi", "Vi", "V2i", "Qi", "KAi", "VMi", "CLMi", "CONC", "CONCM", "CONCT" ),
    obs = list(
      variable = c( "CONC", "CONCM", "CONC", "CONCM" )
    ),
    covariates = covs
  )
  regimen <- new_regimen(amt = 3.7, n=10, interval = 24, type = "oral", cmt = 1)
  data <- data.frame(
    t = 70,
    y = 1,
    evid = 0,
    loq = 0,
    obs_type = 2,
    dv = 1
  )
  t_obs <- seq(0, 50, .5)
  pop <- sim(
    ode = mod,
    regimen = regimen,
    parameters = pars,
    covariates = covs,
    t_obs = rep(t_obs, each = 4),
    only_obs = TRUE,
    obs_type = rep(1:4, length(t_obs))
  )
  # correct obs_types
  expect_equal(
    pop[pop$t %in% c(23, 47), ]$obs_type,
    c(1,2,3,4,1,2,3,4)
  )
  # correct output values
  expect_equal(
    round(pop[pop$t %in% c(23, 47), ]$y, 2),
    c(0.52, 12.34, 0.52, 12.34, 0.63, 17.17, 0.63, 17.17)
  )

})
