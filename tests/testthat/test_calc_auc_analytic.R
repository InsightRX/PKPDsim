## Create reference data:
reg <- new_regimen(amt = 1500, n = 10, interval = 24, type = "infusion")
run_ref <- FALSE
if(run_ref) {
  library(pkvancothomson)
  par <- pkvancothomson::parameters()
  mod <- pkvancothomson::model()
  covs <- list(
    WT = new_covariate(70),
    CRCL = new_covariate(4),
    CL_HEMO = new_covariate(0)
  )
  t_obs <- seq(0, 24*10, 24)
  res <- sim(
    mod,
    parameters = par,
    regimen = reg,
    covariates = covs,
    t_obs = t_obs
  )
  res <- res[res$comp == attr(mod, "size"),]
  res$auc <- round(c(0, diff(res$y)), 3)
  res <- res[, c("t", "auc")]
  par_eff <- PKPDsim::calculate_parameters(mod, parameters = par, covariates = covs)
  parameters = list(CL = par_eff$CLi, V = par_eff$Vi, Q = par_eff$Qi, V2 = par_eff$V2i)
} else {
  res <- data.frame(
    t = c(0, 24, 48, 72, 96, 120, 144, 168, 192, 216, 240),
    auc = c(0, 299.084, 381.493, 427.152, 454.601, 471.194, 481.227, 487.294,
            490.962, 493.181, 494.522)
  )
  parameters <- list(CL = 3.02069794728, V = 47.25, Q = 2.28, V2 = 51.24)
}

## Calculate AUC using linear compartmental models
test_that("AUC corresponds to AUC from ODE model", {
  tmp <- calc_auc_analytic(
    f = "2cmt_iv_infusion",
    regimen = reg,
    parameters = parameters,
    t_obs = res$t
  )
  expect_equal(round(tmp$auc, 3), res$auc)
})
test_that("Can specify using dose/interval/t_inf", {
  tmp <- calc_auc_analytic(
    f = "2cmt_iv_infusion",
    dose = 1500,
    interval = 24,
    t_inf = 1,
    parameters = parameters,
    t_obs = res$t
  )
  expect_equal(round(tmp$auc, 3), res$auc)
})

test_that("Can specify t_obs", {
  tmp <- calc_auc_analytic(
    f = "2cmt_iv_infusion",
    dose = 1500,
    interval = 24,
    t_inf = 1,
    parameters = parameters,
    t_obs = c(72, 96)
  )
  expect_equal(round(tmp$auc, 3), c(sum(res$auc[1:4]), res$auc[5]))
})

test_that("Works with duplicated t_obs", {
  tmp <- calc_auc_analytic(
    f = "2cmt_iv_infusion",
    dose = 1500,
    interval = 24,
    t_inf = 1,
    parameters = parameters,
    t_obs = c(0, 72, 72, 96, 96)
  )
  expect_equal(
    round(tmp$auc, 3),
    c(0, rep(sum(res$auc[1:4]), 2), rep(res$auc[5], 2)))
})

test_that("Works for bolus", {
  tmp <- calc_auc_analytic(
    f = "2cmt_iv_bolus",
    dose = 1500,
    interval = 24,
    parameters = parameters,
    t_obs = res$t
  )
  # AUC should now all be slightly higher than for infusion (but less than 5%)
  expect_true(
    all(tmp$auc[-1] > res$auc[-1] &
      tmp$auc[-1] < res$auc[-1]*1.05)
  )
})

test_that("Works for 1cmt model", {
  tmp <- calc_auc_analytic(
    f = "1cmt_iv_bolus",
    dose = 1500,
    interval = 24,
    parameters = parameters,
    t_obs = res$t
  )
  ## can check AUC very simply now using "fraction of ss":
  AUCss <- 1500 / parameters$CL
  kel <- parameters$CL / parameters$V
  aucfr <- data.frame(
    t = res$t,
    auc = AUCss * (1 - exp(-res$t * kel))
  )
  expect_equal(tmp$auc, aucfr$auc)
})

test_that("Doesn't fail when t_inf is specified as 0 in regimen or as t_inf. Should be nearly equal to bolus", {
  reg1 <- new_regimen(amt = 1500, n = 10, interval = 24, type = "infusion", t_inf = 0)
  reg2 <- new_regimen(amt = 1500, n = 10, interval = 24, type = "bolus")
  tmp1 <- calc_auc_analytic(
    # setting t_inf to non-zero is handled by new_regimen in this case, will set to 1/60, not 1e-6 like in calc_auc_analytic().
    f = "1cmt_iv_infusion",
    parameters = parameters,
    regimen = reg1,
    t_obs = res$t
  )
  tmp2 <- calc_auc_analytic(
    f = "1cmt_iv_bolus",
    parameters = parameters,
    regimen = reg2,
    t_obs = res$t
  )
  tmp3 <- calc_auc_analytic(
    f = "1cmt_iv_bolus",
    parameters = parameters,
    dose = 1500,
    interval = 24,
    t_inf = 0,
    t_obs = res$t
  )
  expect_equal(round(tmp1$auc, 1), round(tmp2$auc, 1))
  expect_equal(round(tmp3$auc, 1), round(tmp2$auc, 1))
})
