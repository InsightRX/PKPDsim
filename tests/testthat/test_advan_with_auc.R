# Uses models and parameters defined in setup.R (conditional, NOT_CRAN only):
# - mod_1cmt_auc, mod_2cmt_auc, mod_3cmt_auc
# - parameters_advan_auc

if (identical(Sys.getenv("NOT_CRAN"), "true")) {
  dose <- 100
  interval <- 12
  n_days <- 5
  t_inf <- 1.5
  t_obs <- c(3, 6, 8, 23, 47)

  ## bolus dataset
  reg_bolus <- new_regimen(
    amt = dose,
    times = seq(0, interval * n_days * (24/interval), interval),
    t_inf = t_inf,
    type = "bolus"
  )
  data_bolus <- advan_create_data(
    reg_bolus,
    parameters = parameters_advan_auc,
    cmts = 5,
    t_obs = t_obs
  )

  ## Infusion dataset
  reg_infusion <- new_regimen(
    amt = dose,
    times = seq(0, interval * n_days * (24/interval), interval),
    t_inf = t_inf,
    type = "infusion"
  )
  data_infusion <- advan_create_data(
    reg_infusion,
    parameters = parameters_advan_auc,
    cmts = 6,
    t_obs = t_obs
  )
}

test_that("One compartment bolus ADVAN runs", {
  skip_on_cran()
  res1_iv_r <- advan("1cmt_iv_bolus", cpp=FALSE)(data_bolus)
  res1_iv_c <- advan("1cmt_iv_bolus", cpp=TRUE)(data_bolus)
  res1_iv_ode <- sim(ode = mod_1cmt_auc, regimen = reg_bolus, parameters = parameters_advan_auc, t_obs = t_obs)

  # AUC R
  expect_equal(round(res1_iv_r[res1_iv_r$TIME %in% t_obs,]$AUC, 5), round(res1_iv_ode[res1_iv_ode$comp == 2,]$y, 5))

  #AUC-C
  expect_equal(round(res1_iv_c[res1_iv_c$TIME %in% t_obs,]$AUC, 5), round(res1_iv_ode[res1_iv_ode$comp == 2,]$y, 5))
})

test_that("Two compartment bolus ADVAN runs", {
  skip_on_cran()
  res2_iv_r <- advan("2cmt_iv_bolus", cpp=FALSE)(data_bolus)
  res2_iv_c <- advan("2cmt_iv_bolus", cpp=TRUE)(data_bolus)
  res2_iv_ode <- sim(ode = mod_2cmt_auc, regimen = reg_bolus, parameters = parameters_advan_auc, t_obs = t_obs)
  expect_equal(
    round(res2_iv_r[res2_iv_r$TIME %in% t_obs,]$AUC, 5),
    round(res2_iv_c[res2_iv_c$TIME %in% t_obs,]$AUC, 5)
  )
  # AUC R
  expect_equal(
    round(res2_iv_r[res2_iv_r$TIME %in% t_obs,]$AUC, 5),
    round(res2_iv_ode[res2_iv_ode$comp == 3,]$y, 5)
  )

  #AUC-C
  expect_equal(
    round(res2_iv_c[res2_iv_c$TIME %in% t_obs,]$AUC, 5),
    round(res2_iv_ode[res2_iv_ode$comp == 3,]$y, 5)
  )
})

test_that("Two compartment infusion ADVAN runs", {
  skip_on_cran()
  res2_inf_r <- advan("2cmt_iv_infusion", cpp=FALSE)(data_infusion)
  res2_inf_c <- advan("2cmt_iv_infusion", cpp=TRUE)(data_infusion)
  res2_inf_ode <- sim(ode = mod_2cmt_auc, regimen = reg_infusion, parameters = parameters_advan_auc, t_obs = t_obs)

  expect_equal(
    round(res2_inf_r[res2_inf_r$TIME %in% t_obs,]$AUC, 5),
    round(res2_inf_c[res2_inf_c$TIME %in% t_obs,]$AUC, 5)
  )

  # AUC R
  expect_equal(
    round(res2_inf_r[res2_inf_r$TIME %in% t_obs,]$AUC, 5),
    round(res2_inf_ode[res2_inf_ode$comp == 3,]$y, 5)
  )

  #AUC-C
  expect_equal(
    round(res2_inf_c[res2_inf_c$TIME %in% t_obs,]$AUC, 5),
    round(res2_inf_ode[res2_inf_ode$comp == 3,]$y, 5)
  )

})

test_that("Three compartment bolus ADVAN runs", {
  skip_on_cran()
  res3_iv_r <- advan("3cmt_iv_bolus", cpp=FALSE)(data_bolus)
  res3_iv_c <- advan("3cmt_iv_bolus", cpp=TRUE)(data_bolus)
  res3_iv_ode <- sim(ode = mod_3cmt_auc, regimen = reg_bolus, parameters = parameters_advan_auc, t_obs = t_obs)
  expect_equal(
    round(res3_iv_r[res3_iv_r$TIME %in% t_obs,]$AUC, 5),
    round(res3_iv_c[res3_iv_c$TIME %in% t_obs,]$AUC, 5)
  )
  # AUC R
  expect_equal(
    round(res3_iv_r[res3_iv_r$TIME %in% t_obs,]$AUC, 5),
    round(res3_iv_ode[res3_iv_ode$comp == 4,]$y, 5)
  )

  #AUC-C
  expect_equal(
    round(res3_iv_c[res3_iv_c$TIME %in% t_obs,]$AUC, 5),
    round(res3_iv_ode[res3_iv_ode$comp == 4,]$y, 5)
  )
})

test_that("Three compartment iv ADVAN runs", {
  skip_on_cran()
  res3_iv_r <- advan("3cmt_iv_infusion", cpp=FALSE)(data_infusion)
  res3_iv_c <- advan("3cmt_iv_infusion", cpp=TRUE)(data_infusion)
  res3_iv_ode <- sim(ode = mod_3cmt_auc, regimen = reg_infusion, parameters = parameters_advan_auc, t_obs = t_obs)
  expect_equal(
    round(res3_iv_r[res3_iv_r$TIME %in% t_obs,]$AUC, 5),
    round(res3_iv_c[res3_iv_c$TIME %in% t_obs,]$AUC, 5)
  )
  # AUC R
  expect_equal(
    round(res3_iv_r[res3_iv_r$TIME %in% t_obs,]$AUC, 5),
    round(res3_iv_ode[res3_iv_ode$comp == 4,]$y, 5)
  )

  #AUC-C
  expect_equal(
    round(res3_iv_c[res3_iv_c$TIME %in% t_obs,]$AUC, 5),
    round(res3_iv_ode[res3_iv_ode$comp == 4,]$y, 5)
  )
})
