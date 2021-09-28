## models: shared between tests and take a while to compile
#  - oral models
## Uses model defined in setup.R
pk1cmt_oral_anal = function(t, dose, KA, V, CL) {
  dose*KA/(V*(KA-CL/V))*(exp(-(CL/V) * t)-exp(-KA * t))
}
pk1cmt_oral_code <- new_ode_model(
  code = "dAdt[1] = -KA*A[1]; dAdt[2] = KA*A[1] - (CL/V)*A[2];",
  obs=list(cmt = 2, scale="V")
)

#   - iv models
## Uses model defined in setup.R

#   - model with dose cmt specified
dose_in_cmt_2  <- new_ode_model(
  code = "
      dAdt[1] = -KA * A[1];
      dAdt[2] = KA*A[1] -(CL/V) * A[2]
      dAdt[3] = S2*(A[2]-A[3])
    ",
  obs = list(cmt=2, scale="V"),
  dose = list(cmt = 2),
  cpp_show_code = FALSE
)


test_that("Library and custom C++ and code matches analytic soln", {
  p <- list(KA = 1, CL = 5, V = 50)
  t_obs <- c(0:72)
  t_obs2 <- t_obs + 0.1234 # also needs to be producing results with non-integer times
  dose <- 100
  t_dose <- c(0)
  regimen <- new_regimen(amt=dose, times = t_dose, type = "oral")

  pk1cmt_oral_lib <- sim_ode(
    ode = mod_1cmt_oral,
    parameters = p,
    regimen = regimen,
    t_obs = t_obs,
    int_step_size = 0.1,
    duplicate_t_obs = TRUE,
    only_obs=TRUE
  )

  pk1cmt_oral_code <- sim_ode(
    ode = pk1cmt_oral_code,
    parameters = p,
    duplicate_t_obs = TRUE,
    regimen=regimen,
    t_obs=t_obs,
    int_step_size = 0.1,
    only_obs=TRUE
  )

  pk1cmt_oral_anal_res <- pk1cmt_oral_anal(t_obs, dose, p$KA, p$V, p$CL)
  expect_equal(round(pk1cmt_oral_lib$y, 3), round(pk1cmt_oral_anal_res, 3))
  expect_equal(round(pk1cmt_oral_code$y, 3), round(pk1cmt_oral_anal_res, 3))
})


test_that("precision in time does not impact # obs returned", {
  regimen_mult <- new_regimen(
    amt = rep(12.8, 3),
    times = c(0, 6, 12),
    type="infusion",
    t_inf = 2
  )
  t_obs <- c(11.916, 14.000, 16.000, 17.000, 30)
  tmp <- sim_ode(
    ode = mod_1cmt_iv,
    parameters = list(CL = 5, V = 50),
    regimen = regimen_mult,
    t_obs = t_obs,
    only_obs = TRUE
  )
  expect_equal(tmp$t, t_obs)
})

test_that("test bug EmCo 20150925", {
  xtim <- c(0, 2, 4, 8, 12, 24)
  sujdos <- 320
  param <- list(KA = 1.8, V = 30, CL = 1.7)
  regim <- new_regimen(amt = sujdos, times = c(0, 12), type= "bolus")
  out <- sim_ode(ode = mod_1cmt_oral, parameters=param, regimen=regim, t_obs = xtim, only_obs = TRUE)
  expect_equal(out$t, xtim)
})

test_that("model size is appropriate (bug: JeHi 20151204)", {
  pk3cmt <- new_ode_model(
    code = "
      dAdt[1] = -KA*A[1];
      dAdt[2] = KA*A[1] -(Q/V)*A[2] + (Q/V2)*A[3] -(CL/V)*A[2];
      dAdt[3] = -(Q/V2)*A[3] + (Q/V)*A[2];
      ",
    obs = list(cmt = 2, scale = "V")
  )
  expect_equal( attr(pk3cmt, "size"), 3)
})

test_that("Dose is added to correct compartment: specified by model", {
  set.seed(90)
  p <- list(CL = 1, V  = 10, KA = 0.5, S2 = .1)
  r <- new_regimen(amt = 100, times = c(0), type = "infusion")
  dat <- sim_ode(
    ode = dose_in_cmt_2,
    n_ind = 1,
    omega = cv_to_omega(par_cv = list("CL"=0.1, "V"=0.1, "KA" = .1), p),
    parameters = p,
    regimen = r,
    verbose = FALSE,
    t_max = 48
  )
  # Dose should be in cmt 2
  expect_equal(dat$y[dat$comp == 1], rep(0, 50))
  expect_true(all(dat$y[dat$comp == 2][-1] > 0))
})

test_that("Dose is added to correct compartment: override model by regimen", {
  set.seed(60)
  p <- list(CL = 1, V  = 10, KA = 0.5, S2 = .1)
  r <- new_regimen(
    amt = c(100, 100, 100),
    times = c(0, 6, 12),
    cmt = c(1,2,3),
    type = "bolus"
  )
  dat <- sim_ode(
    ode = dose_in_cmt_2,
    n_ind = 1,
    omega = cv_to_omega(par_cv = list("CL"=0.1, "V"=0.1, "KA" = .1), p),
    parameters = p,
    regimen = r,
    verbose = FALSE,
    t_max = 48
  )
  # Dose should be in cmt 1, 2 and 3
  expect_true(all(dat$y[dat$comp == 1 & dat$t > 0] > 0))
  expect_true(max(diff(dat$y[dat$comp == 2])) > 95)
  expect_true(max(diff(dat$y[dat$comp == 3])) > 95)
})

test_that("Infusion works for all compartments", {
  set.seed(44)
  # Part 1: Specify cmt only with model
  p <- list(CL = 1, V  = 10, KA = 0.5, S2 = .1)
  r <- new_regimen(
    amt = c(100, 100, 100),
    times = c(0, 6, 12),
    cmt = c(1,2,3),
    t_inf = 3,
    type = "infusion"
  )
  dat <- sim_ode(
    ode = dose_in_cmt_2,
    n_ind = 1,
    omega = cv_to_omega(par_cv = list("CL"=0.1, "V"=0.1, "KA" = .1), p),
    parameters = p,
    regimen = r,
    verbose = FALSE,
    t_max = 48
  )
  expect_true(all(dat$y[dat$comp == 1 & dat$t > 0 ] > 0))
  expect_true(max(diff(dat$y[dat$comp == 2])) > 25)
  expect_true(max(diff(dat$y[dat$comp == 3])) > 25)
  expect_equal(round(max(dat$y[dat$comp == 2]), 1), 131.2)
  expect_equal(round(max(dat$y[dat$comp == 3]), 1), 148.4)
})

test_that("Duplicate obs returned when specified in arg", {
  # for first 2 doses, infusion time will just be ignored, but a value has to be specified in the vector
  p <- list(CL = 1, V  = 10, KA = 0.5, S2=.1)
  r <- new_regimen(
    amt = c(100, 100, 100, 100),
    times = c(0, 6, 12, 18),
    cmt = c(2, 2, 1, 1),
    t_inf = c(1, 1, 1, 1),
    type = c("bolus", "bolus", "infusion", "infusion")
  )
  dat <- sim_ode(
    ode = mod_1cmt_oral,
    n_ind = 1,
    omega = cv_to_omega(par_cv = list("CL"=0.1, "V"=0.1, "KA" = .1), p),
    parameters = p,
    regimen = r,
    t_obs = c(1, 2, 3, 4, 4, 4, 6), ## see duplicate obs here
    duplicate_t_obs = T,
    only_obs = FALSE
  )
  expect_equal(length(dat[dat$t == 4,]$y),  9)
  expect_equal(length(dat$y), 21)
  expect_equal(sum(is.na(dat$y)), 0)
})

test_that("Custom t_obs is returned", {
  t_obs <- seq(from = 0, to = 24, by = .1)
  p <- list(CL = 1, V  = 10, KA = 0.5, S2=.1)
  r <- new_regimen(
    amt = c(100, 100, 100, 100),
    times = c(0, 6, 12, 18),
    cmt = c(2, 2, 1, 1),
    t_inf = c(1, 1, 1, 1),
    type = c("bolus", "bolus", "infusion", "infusion")
  )
  dat <- sim_ode(
    ode = mod_1cmt_oral,
    n_ind = 1,
    omega = cv_to_omega(par_cv = list("CL"=0.1, "V"=0.1, "KA" = .1), p),
    parameters = p,
    regimen = r,
    t_obs = t_obs,
    only_obs = T
  )
  expect_equal(mean(diff(t_obs)), mean(diff(dat$t)))
})

test_that("if covariate time is at end of infusion, end of infusion is still recorded", {
  # Bug reported by JF
  pop_est <- list(CL = 1.08, V = 0.98)
  regimen <- new_regimen(
    amt = c(1500, 1000, 1500, 1500, 1500, 1500, 1500),
    type = "infusion",
    t_inf = c(2, 1, 2, 2, 1, 1, 1),
    times = c(0, 10.8666666666667, 20.4333333333333, 32.0666666666667, 46.9, 54.9, 62.9 )
  )
  covs <- list(
    WT = new_covariate(value = c(60, 65), times = c(0, 47.9)),
    CRCL = new_covariate(8), CVVH = new_covariate(0)
  )
  pksim <- sim(
    ode = mod_1cmt_iv,
    parameters = pop_est,
    covariates = covs,
    regimen = regimen,
    checks = TRUE,
    only_obs = TRUE
  )
  expect_true(all(pksim$y < 1000))
})

test_that("Covariate table simulation runs", {
  # this test used to be in the covariate_table_to_list file but
  # makes more sense here.
  p <- list(CL = 5, V = 50)
  reg <- new_regimen (amt = 100, n = 4, interval = 12, type = "bolus",  cmt=1)
  om <- c(0.01, 1, 0.01)
  cov_table <- data.frame(
    id=c(1, 1, 2, 3),
    WT = c(40, 45, 50, 60),
    SCR = c(50, 150, 90,110),
    t = c(0, 5, 0, 0)
  )

  dat <- sim(
    mod_1cmt_iv,
    parameters = p,
    regimen = reg,
    covariates_table = cov_table,
    covariates_implementation = list(SCR = "interpolate"),
    omega = NULL,
    n_ind = 3,
    only_obs = T,
    output_include = list(parameters = TRUE, covariates=TRUE)
  )
  expect_equal(length(unique(dat$id)), 3)
})
