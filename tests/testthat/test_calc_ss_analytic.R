# shared parameters
dose <- 100
interval <- 12
n_ss <- 100
reg_oral <- new_regimen(amt = dose, interval = interval, n = n_ss, type = "oral")
reg_bolus <- new_regimen(amt = dose, interval = interval, n = n_ss, type = "bolus")
reg_inf <- new_regimen(amt = dose, interval = interval, n = n_ss, type = "infusion")
t_obs <- max(reg_oral$dose_times) + interval
# Uses models defined in setup.R

if (identical(Sys.getenv("NOT_CRAN"), "true")) {
  pk_3cmt_iv <- new_ode_model("pk_3cmt_iv")
}

#delta <- function(x, ref) { abs(x-ref)/ref }

test_that("1 cmt oral", {
  par <- list(CL = 5, V = 100, KA = 1)
  res_ana <- calc_ss_analytic(f = "1cmt_oral", dose = dose, interval = interval, parameters = par)
  res_ode <- sim(mod_1cmt_oral, parameters = par, regimen = reg_oral, t_obs = t_obs, only_obs = F)$y
  expect_equal( attr(advan("1cmt_oral"), "cmt"), 2)
  expect_equal(res_ana[1:2], res_ode[1:2])
})

test_that("Transit compartments can be added", {
  par <- list(CL = 5, V = 100, KA = 1)
  res_ana_transit <- calc_ss_analytic(
    f = "1cmt_oral",
    dose = dose,
    interval = interval,
    parameters = par,
    n_transit_compartments = 3
  )
  res_ana_auc <- calc_ss_analytic(
    f = "1cmt_oral",
    dose = dose,
    interval = interval,
    parameters = par,
    auc = TRUE
  )
  res_ana_both <- calc_ss_analytic(
    f = "1cmt_oral",
    dose = dose,
    interval = interval,
    parameters = par,
    auc = TRUE,
    n_transit_compartments = 3
  )
  expect_equal(length(res_ana_transit), 5)
  expect_equal(length(res_ana_auc), 3)

  # transit comps and AUC comps added
  expect_equal(length(res_ana_both), 6)
  expect_equal(sum(res_ana_both[c(2,3,6)]), 0)
})

test_that("1-cmt iv bolus", {
  par <- list(CL = 5, V = 100)
  res_ana <- calc_ss_analytic(f = "1cmt_iv_bolus", dose = dose, interval = interval, parameters = par)
  res_ode <- sim(mod_1cmt_iv, parameters = par, regimen = reg_bolus, t_obs = t_obs, only_obs = F, duplicate_t_obs = FALSE)$y
  expect_equal(attr(advan("1cmt_iv_bolus"), "cmt"), 1)
  expect_equal(res_ana[1], res_ode[1])
})

test_that("1-cmt iv infusion", {
  par <- list(CL = 5, V = 100, KA = 1)
  res_ana <- calc_ss_analytic(f = "1cmt_iv_infusion", dose = dose, interval = interval, t_inf = 1, parameters = par)
  res_ode <- sim(mod_1cmt_iv, parameters = par, regimen = reg_inf, t_obs = t_obs, only_obs = F)$y
  expect_equal(attr(advan("1cmt_iv_bolus"), "cmt"), 1)
  expect_equal(res_ana[1], res_ode[1])
})

test_that("2-cmt oral", {
  skip_on_cran()
  pk_2cmt_oral <- new_ode_model("pk_2cmt_oral")
  par <- list(CL = 5, V = 100, Q = 3, V2 = 150, KA = 1)
  res_ana <- calc_ss_analytic(f = "2cmt_oral", dose = dose, interval = interval, parameters = par)
  res_ode <- sim(pk_2cmt_oral, parameters = par, regimen = reg_oral, t_obs = t_obs, only_obs = F)$y
  expect_equal(attr(advan("2cmt_oral"), "cmt"), 3)
  expect_equal(signif(res_ana[1:3], 5), signif(res_ode[1:3], 5))
})

test_that("2-cmt iv bolus", {
  par <- list(CL = 5, V = 100, Q = 3, V2 = 150)
  res_ana <- calc_ss_analytic(f = "2cmt_iv_bolus", dose = dose, interval = interval, parameters = par)
  res_ode <- sim(mod_2cmt_iv, parameters = par, regimen = reg_bolus, t_obs = t_obs, only_obs = F)$y
  expect_equal(attr(advan("2cmt_iv_bolus"), "cmt"), 2)
  expect_equal(signif(res_ana[1], 5), signif(res_ode[1], 5))
})

test_that("2-cmt infusion", {
  par <- list(CL = 5, V = 100, Q = 3, V2 = 150)
  res_ana <- calc_ss_analytic(f = "2cmt_iv_infusion", dose = dose, interval = interval, parameters = par, t_inf = 1)
  res_ode <- sim(mod_2cmt_iv, parameters = par, regimen = reg_inf, t_obs = t_obs, only_obs = F)$y
  expect_equal(attr(advan("2cmt_iv_bolus"), "cmt"), 2)
  expect_equal(signif(res_ana[1], 5), signif(res_ode[1], 5))
})

test_that("3-cmt oral", {
  skip_on_cran()
  pk_3cmt_oral <- new_ode_model("pk_3cmt_oral")
  par <- list(CL = 5, V = 100, Q = 3, V2 = 150, Q2 = 6, V3 = 250, KA = 1)
  res_ana <- calc_ss_analytic(f = "3cmt_oral", dose = dose, interval = interval, parameters = par)
  res_ode <- sim(pk_3cmt_oral, parameters = par, regimen = reg_oral, t_obs = t_obs, only_obs = F)$y
  expect_equal(attr(advan("3cmt_oral"), "cmt"), 4)
  expect_equal(signif(res_ana[1:3], 3), signif(res_ode[1:3], 3))
})

test_that("3-cmt iv bolus", {
  skip_on_cran()
  par <- list(CL = 5, V = 100, Q = 3, V2 = 150, Q2 = 6, V3 = 250)
  res_ana <- calc_ss_analytic(f = "3cmt_iv_bolus", dose = dose, interval = interval, parameters = par)
  res_ode <- sim(pk_3cmt_iv, parameters = par, regimen = reg_bolus, t_obs = t_obs, only_obs = F)$y
  expect_equal(attr(advan("3cmt_iv_bolus"), "cmt"), 3)
  expect_equal(signif(res_ana[1],4),  signif(res_ode[1], 4))
})

test_that("3-cmt infusion", {
  skip_on_cran()
  par <- list(CL = 5, V = 100, Q = 3, V2 = 150, Q2 = 6, V3 = 252)
  res_ana <- calc_ss_analytic(f = "3cmt_iv_infusion", dose = dose, interval = interval, parameters = par, t_inf = 1)
  res_ode <- sim(pk_3cmt_iv, parameters = par, regimen = reg_inf, t_obs = t_obs, only_obs = F)$y
  expect_equal(attr(advan("3cmt_iv_bolus"), "cmt"), 3)
  expect_equal(signif(res_ana[1], 3), signif(res_ode[1], 3))
})

test_that("covariate re-map", {
  par <- list(Ke = 5, V = 100)
  expect_error(
    calc_ss_analytic(
      f = "1cmt_iv_bolus",
      dose = dose,
      interval = interval,
      parameters = par,
      map = NULL
    )
  )
  expect_error(
    calc_ss_analytic(
      f = "1cmt_iv_bolus",
      dose = dose,
      interval = interval,
      parameters = par,
      map = list("CL" = "Ke")
    ),
    NA
  )
})
