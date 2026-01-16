# ---- Shared Setup ----
## These models are also tested in the unit tests for `calc_ss_analytics()`, so just testing a few example cases here
dose <- 100
interval <- 12
t_inf <- 1
n_days <- 5
parameters <- list(CL = 10, V = 50, KA = 0.5, Q = 5, V2 = 100, Q2 = 3, V3 = 150, F1 = 1)
t_obs <- c(3, 6, 8, 23)
reg_bolus <- new_regimen(
  amt = dose,
  times = seq(0, interval * n_days * (24/interval), interval),
  t_inf = t_inf, type = "bolus"
)
data <- advan_create_data(
  reg_bolus,
  parameters = parameters,
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
  parameters = parameters,
  cmts = 6,
  t_obs = t_obs
)

# ---- One Compartment Tests ----
test_that("One compartment IV bolus", {
  res1_iv   <- advan("1cmt_iv_bolus", cpp=FALSE)(data)
  res1_iv_c <- advan("1cmt_iv_bolus", cpp=TRUE)(data)
  expect_equal(round(res1_iv[res1_iv$TIME == 23,]$DV, 3), 0.242)
  expect_true(!any(is.na(res1_iv$DV)))
  expect_equal(res1_iv, res1_iv_c)
})

test_that("One compartment IV infusion", {
  res1_iv_inf   <- advan("1cmt_iv_infusion", cpp=FALSE)(data_infusion)
  res1_iv_inf_c <- advan("1cmt_iv_infusion", cpp=TRUE)(data_infusion)
  f1 <- advan("1cmt_iv_infusion", cpp=FALSE)
  f2 <- advan("1cmt_iv_infusion", cpp=TRUE)

  expect_equal(round(res1_iv_inf[res1_iv_inf$TIME == 23,]$DV, 3), 0.268)
  expect_true(!any(is.na(res1_iv_inf$DV)))
  expect_equal(res1_iv_inf, res1_iv_inf_c)
  expect_equal(attr(f1, "type"), "infusion")
  expect_equal(attr(f2, "type"), "infusion")
  expect_equal(attr(f1, "implementation"), FALSE)
  expect_equal(attr(f2, "implementation"), TRUE)
  expect_equal(attr(f1, "cmt"), 1)
  expect_equal(attr(f2, "cmt"), 1)
})

test_that("One compartment oral", {
  res1_oral <- advan("1cmt_oral", cpp=FALSE)(data)
  res1_oral_c <- advan("1cmt_oral", cpp=TRUE)(data)

  expect_equal(round(res1_oral[res1_oral$TIME == 23,]$DV, 3), 0.389)
  expect_true(!any(is.na(res1_oral$DV)))
  expect_equal(res1_oral, res1_oral_c)
})

# ---- Two Compartment Tests ----
test_that("Two compartment iv bolus", {
  res2_iv   <- advan("2cmt_iv_bolus", cpp=FALSE)(data)
  res2_iv_c <- advan("2cmt_iv_bolus", cpp=TRUE)(data)

  expect_equal(round(res2_iv[res2_iv$TIME == 23,]$DV, 3), 0.212)
  expect_true(!any(is.na(res2_iv$DV)))
  expect_equal(res2_iv, res2_iv_c)
})

test_that("Two compartment iv infusion", {
  res2_iv_inf   <- advan("2cmt_iv_infusion", cpp=FALSE)(data_infusion)
  res2_iv_inf_c <- advan("2cmt_iv_infusion", cpp=TRUE)(data_infusion)

  expect_equal(round(res2_iv_inf[res2_iv_inf$TIME == 23,]$DV, 3), 0.225)
  expect_true(!any(is.na(res2_iv_inf$DV)))
  expect_equal(res2_iv_inf, res2_iv_inf_c)
})

test_that("Two compartment oral", {
  res2_oral   <- advan("2cmt_oral", cpp=FALSE)(data)
  res2_oral_c <- advan("2cmt_oral", cpp=TRUE)(data)

  expect_equal(round(res2_oral[res2_oral$TIME == 23,]$DV, 3), 0.302)
  expect_true(!any(is.na(res2_oral$DV)))
  expect_equal(res2_oral, res2_oral_c)
})

# ---- Three Compartment Tests ----
test_that("Three compartment IV bolus", {
  res3_iv   <- advan("3cmt_iv_bolus", cpp=FALSE)(data)
  res3_iv_c <- advan("3cmt_iv_bolus", cpp=TRUE)(data)

  expect_equal(round(res3_iv[res3_iv$TIME == 23,]$DV, 3), 0.169)
  expect_true(!any(is.na(res3_iv$DV)))
  expect_equal(res3_iv, res3_iv_c)
})

test_that("Three compartment IV infusion", {
  res3_iv_inf   <- advan("3cmt_iv_infusion", cpp=FALSE)(data_infusion)
  res3_iv_inf_c <- advan("3cmt_iv_infusion", cpp=TRUE)(data_infusion)

  expect_equal(round(res3_iv_inf[res3_iv_inf$TIME == 23,]$DV, 3), 0.177)
  expect_true(!any(is.na(res3_iv_inf$DV)))
  expect_equal(res3_iv_inf, res3_iv_inf_c)
})

test_that("Three compartment IV oral", {
  res3_oral <- advan("3cmt_oral", cpp=FALSE)(data)
  res3_oral_c <- advan("3cmt_oral", cpp=TRUE)(data)

  expect_equal(round(res3_oral[res3_oral$TIME == 23,]$DV, 3), 0.236)
  expect_true(!any(is.na(res3_oral$DV)))
  expect_equal(res3_oral, res3_oral_c)
})

# ---- AUC Comparison Tests (NOT_CRAN) ----
if (identical(Sys.getenv("NOT_CRAN"), "true")) {
  # Setup for AUC tests
  t_obs_auc <- c(3, 6, 8, 23, 47)
  t_inf_auc <- 1.5

  ## bolus dataset for AUC tests
  reg_bolus_auc <- new_regimen(
    amt = dose,
    times = seq(0, interval * n_days * (24/interval), interval),
    t_inf = t_inf_auc,
    type = "bolus"
  )
  data_bolus_auc <- advan_create_data(
    reg_bolus_auc,
    parameters = parameters,
    cmts = 5,
    t_obs = t_obs_auc
  )

  ## Infusion dataset for AUC tests
  reg_infusion_auc <- new_regimen(
    amt = dose,
    times = seq(0, interval * n_days * (24/interval), interval),
    t_inf = t_inf_auc,
    type = "infusion"
  )
  data_infusion_auc <- advan_create_data(
    reg_infusion_auc,
    parameters = parameters,
    cmts = 6,
    t_obs = t_obs_auc
  )
}

test_that("One compartment bolus ADVAN runs", {
  skip_on_cran()
  res1_iv_r <- advan("1cmt_iv_bolus", cpp=FALSE)(data_bolus_auc)
  res1_iv_c <- advan("1cmt_iv_bolus", cpp=TRUE)(data_bolus_auc)
  res1_iv_ode <- sim(ode = mod_1cmt_iv_auc, regimen = reg_bolus_auc, parameters = parameters, t_obs = t_obs_auc)

  # AUC R
  expect_equal(round(res1_iv_r[res1_iv_r$TIME %in% t_obs_auc,]$AUC, 5), round(res1_iv_ode[res1_iv_ode$comp == 2,]$y, 5))

  #AUC-C
  expect_equal(round(res1_iv_c[res1_iv_c$TIME %in% t_obs_auc,]$AUC, 5), round(res1_iv_ode[res1_iv_ode$comp == 2,]$y, 5))
})

test_that("Two compartment bolus ADVAN runs", {
  skip_on_cran()
  res2_iv_r <- advan("2cmt_iv_bolus", cpp=FALSE)(data_bolus_auc)
  res2_iv_c <- advan("2cmt_iv_bolus", cpp=TRUE)(data_bolus_auc)
  res2_iv_ode <- sim(ode = mod_2cmt_iv_auc, regimen = reg_bolus_auc, parameters = parameters, t_obs = t_obs_auc)
  expect_equal(
    round(res2_iv_r[res2_iv_r$TIME %in% t_obs_auc,]$AUC, 5),
    round(res2_iv_c[res2_iv_c$TIME %in% t_obs_auc,]$AUC, 5)
  )
  # AUC R
  expect_equal(
    round(res2_iv_r[res2_iv_r$TIME %in% t_obs_auc,]$AUC, 5),
    round(res2_iv_ode[res2_iv_ode$comp == 3,]$y, 5)
  )

  #AUC-C
  expect_equal(
    round(res2_iv_c[res2_iv_c$TIME %in% t_obs_auc,]$AUC, 5),
    round(res2_iv_ode[res2_iv_ode$comp == 3,]$y, 5)
  )
})

test_that("Two compartment infusion ADVAN runs", {
  skip_on_cran()
  res2_inf_r <- advan("2cmt_iv_infusion", cpp=FALSE)(data_infusion_auc)
  res2_inf_c <- advan("2cmt_iv_infusion", cpp=TRUE)(data_infusion_auc)
  res2_inf_ode <- sim(ode = mod_2cmt_iv_auc, regimen = reg_infusion_auc, parameters = parameters, t_obs = t_obs_auc)

  expect_equal(
    round(res2_inf_r[res2_inf_r$TIME %in% t_obs_auc,]$AUC, 5),
    round(res2_inf_c[res2_inf_c$TIME %in% t_obs_auc,]$AUC, 5)
  )

  # AUC R
  expect_equal(
    round(res2_inf_r[res2_inf_r$TIME %in% t_obs_auc,]$AUC, 5),
    round(res2_inf_ode[res2_inf_ode$comp == 3,]$y, 5)
  )

  #AUC-C
  expect_equal(
    round(res2_inf_c[res2_inf_c$TIME %in% t_obs_auc,]$AUC, 5),
    round(res2_inf_ode[res2_inf_ode$comp == 3,]$y, 5)
  )

})

test_that("Three compartment bolus ADVAN runs", {
  skip_on_cran()
  res3_iv_r <- advan("3cmt_iv_bolus", cpp=FALSE)(data_bolus_auc)
  res3_iv_c <- advan("3cmt_iv_bolus", cpp=TRUE)(data_bolus_auc)
  res3_iv_ode <- sim(ode = mod_3cmt_iv_auc, regimen = reg_bolus_auc, parameters = parameters, t_obs = t_obs_auc)
  expect_equal(
    round(res3_iv_r[res3_iv_r$TIME %in% t_obs_auc,]$AUC, 5),
    round(res3_iv_c[res3_iv_c$TIME %in% t_obs_auc,]$AUC, 5)
  )
  # AUC R
  expect_equal(
    round(res3_iv_r[res3_iv_r$TIME %in% t_obs_auc,]$AUC, 5),
    round(res3_iv_ode[res3_iv_ode$comp == 4,]$y, 5)
  )

  #AUC-C
  expect_equal(
    round(res3_iv_c[res3_iv_c$TIME %in% t_obs_auc,]$AUC, 5),
    round(res3_iv_ode[res3_iv_ode$comp == 4,]$y, 5)
  )
})

test_that("Three compartment iv ADVAN runs", {
  skip_on_cran()
  res3_iv_r <- advan("3cmt_iv_infusion", cpp=FALSE)(data_infusion_auc)
  res3_iv_c <- advan("3cmt_iv_infusion", cpp=TRUE)(data_infusion_auc)
  res3_iv_ode <- sim(ode = mod_3cmt_iv_auc, regimen = reg_infusion_auc, parameters = parameters, t_obs = t_obs_auc)
  expect_equal(
    round(res3_iv_r[res3_iv_r$TIME %in% t_obs_auc,]$AUC, 5),
    round(res3_iv_c[res3_iv_c$TIME %in% t_obs_auc,]$AUC, 5)
  )
  # AUC R
  expect_equal(
    round(res3_iv_r[res3_iv_r$TIME %in% t_obs_auc,]$AUC, 5),
    round(res3_iv_ode[res3_iv_ode$comp == 4,]$y, 5)
  )

  #AUC-C
  expect_equal(
    round(res3_iv_c[res3_iv_c$TIME %in% t_obs_auc,]$AUC, 5),
    round(res3_iv_ode[res3_iv_ode$comp == 4,]$y, 5)
  )
})

# ---- Covariate Tests ----
test_that("Analytic and ODE models with covariates are the same", {
  skip_on_cran()

  ## Create dataset
  dose <- 100
  interval <- 12
  n_days <- 2
  parameters <- list(
    CL = 10,
    V = 50,
    KA = 0.5,
    Q = 5,
    V2 = 100,
    Q2 = 3,
    V3 = 150,
    F1 = 1
  )
  t_obs <- seq(0, 40, .1)
  reg_bolus <- new_regimen(
    amt = dose,
    times = seq(0, interval * n_days * (24/interval), interval),
    type = "bolus"
  )
  ## there is slight difference in how bolus doses are handled.
  ## Analytical equation is perhaps more consistent, so not testing
  ## simulations at dose times. Should look into later.
  t_obs <- t_obs[! t_obs %in% reg_bolus$dose_times]
  covariates <- list(WT = new_covariate(80), CRCL=new_covariate(4.5))

  ## Using analytic equations model:
  data_ana <- sim(
    analytical = "1cmt_iv_bolus",
    parameters = parameters,
    covariates = covariates,
    regimen = reg_bolus,
    t_obs = t_obs,
    covariate_model = "CL = CL * (CRCL / 3)^0.75; V = V * (WT / 70.0)"
  )

  ## Using ODE model:
  mod1 <- new_ode_model(
    code = "
    dAdt[1] = -( (CL*pow(CRCL/3.0, 0.75)) / (V*WT/70.0) ) * A[1];
  ",
  covariates = covariates,
  obs = list(cmt = 1, scale = "V*WT/70.0"), dose = list(cmt = 1)
  )
  data_ode <- sim(
    ode = mod1,
    parameters = parameters,
    covariates = covariates,
    regimen = reg_bolus,
    t_obs = t_obs,
    duplicate_t_obs = TRUE,
    only_obs = TRUE
  )

  expect_equal(nrow(data_ana), nrow(data_ode))
  expect_equal(round(data_ana$y,4), round(data_ode$y, 4))
})
