
test_that("Expected parsed regimen structure", {
  expect_cols <- c(
    "t", "dose", "type", "dum", "dose_cmt", "t_inf",
    "evid", "bioav", "rate", "cov_WT",
    "cov_t_WT", "gradients_WT", "obs_type"
  )
  reg_bolus <- new_regimen(
    amt = 100,
    interval = 24,
    n = 5,
    type = "bolus"
  )
  covs <- list(
    WT = new_covariate(
      value = c(80, 100),
      time = c(0, 50),
      implementation = "interpolate"
      )
    )
  res <- create_event_table(
    reg_bolus,
    covariates = covs,
    t_obs = seq(0, 100, 5)
  )

  expect_equal(nrow(res), 26)
  expect_true(all(expect_cols %in% names(res)))
  expect_equal(sum(res$dose), 500)
})

test_that("Rounding-related index matching issues in time col", {
  test_reg <- new_regimen(
    amt = 1500,
    interval = 24,
    times = 145.217,
    type = "infusion",
    t_inf = 1.5
  )
  p <- list(
    CL = 4.5,
    V = 58.4,
    V2 = 38.4,
    Q = 6.5,
    TH_CRCL = 0.8,
    TH_DIAL_CL = 0.7,
    TH_DIAL_V = 0.5,
    TDM_INIT = 17.2
  )
  covs <- list(
    PMA = structure(
      list(
        value = c(3313.24850396581, 3313.39136110867),
        times = c(0, 24),
        implementation = "interpolate",
        unit = c("weeks", "weeks"),
        comments = NULL
      ),
      class = c("covariate", "list" )
    )
  )
  t_obs <- c(
    -145.217, -122.217, -121.217,
    -120.217,   -0.217,        0,
           1,      1.5,       23,
          24,       25,      168
  )
  res2 <- create_event_table(
    test_reg,
    t_init = 145.217,
    t_obs = t_obs,
    covariates = covs
  )
  expect_equal(sum(is.na(res2$cov_PMA)), 0)
})

test_that("Multiple obs_types does not add erroneous infusion stop events", {
  reg <- new_regimen(
    amt = c(100, 100, 100, 100),
    times = c(0, 336, 672, 1008),
    t_inf = 3,
    type = "infusion"
  )
  res <- create_event_table(
    regimen = reg,
    t_max = NULL,
    t_obs = c(3, 3.5, 4, 6, 8, 12, 16, 330, 340, 670, 676, 1005, 1200,
              3, 3.5, 4, 6, 8, 12, 16, 330, 340, 670, 676, 1005, 1200),
    obs_type = c(1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2),
    covariates = list(WT = new_covariate(70)),
    model = NULL
  )
  expect_true(all(cumsum(res$rate) >= 0)) # cumulative dose rate should never be lower than zero
  expect_true(sum(res$rate) == 0) # net dose should always be zero
})

test_that("simulatenous doses in different cmts do not remove infusion stops", {
  reg <- new_regimen(
    amt = c(100, 10, 100, 10, 100, 10),
    times = c(0, 0, 12, 12, 24, 24),
    t_inf = 0.5,
    type = rep(c("drug_1", "drug_2"), 3)
  )
  model <- mod_2cmt_iv
  attr(model, "cmt_mapping") <- list(drug_1 = 1, drug_2 = 2)
  res <- create_event_table(
    regimen = reg,
    t_obs = 36,
    covariates = list(WT = new_covariate(value = c(50, 55), times = c(0, 20))),
    model = model
  )
  expect_true(all(cumsum(res$rate) >= 0)) # cumulative dose rate should never be lower than zero
  expect_true(sum(res$rate) == 0) # net dose should always be zero
  expect_true(all((reg$dose_amts/reg$t_inf) %in% res$rate)) # rates are right
})

test_that("useful cmt_mapping mismatch error is thrown", {
  reg <- new_regimen(
    amt = 100,
    times = c(0, 12, 24),
    t_inf = 0,
    type = rep("oral", 3)
  )
  model <- mod_2cmt_iv
  attr(model, "cmt_mapping") <- list(infusion = 1, bolus = 1) # not oral
  expect_error(
    create_event_table(
      regimen = reg,
      t_obs = 36,
      covariates = list(WT = new_covariate(value = c(50, 55), times = c(0, 20))),
      model = model
    ),
    "Unrecognized regimen type. Define 'oral' in model md field `cmt_mapping`",
    fixed = TRUE
  )
})

test_that("identically-timed but different levels of precision in TDM vs covs", {
  # here, t_init is non-zero with a value of 1.283. Covariate times have also
  # been shifted accordingly. It's possible for a merge issue to insert NAs
  reg <- PKPDsim::new_regimen(
    type = rep("infusion", 10L),
    t_inf = rep(c(2, 1.5), c(1L, 9L)),
    times = c(
      0, 20.0833333333333, 32.31666666666667, 45.63333333333333, 56.6,
      93.18333333333334, 117.083333333333329, 141.85, 166.1, 212.7
    ),
    amt = rep(c(1750, 1500, 1250), c(1L, 3L, 6L)),
  )
  covs <- list(
    CR = PKPDsim::new_covariate(
      value = c(
        0.756, 0.76, 0.71, 0.66, 0.7, 0.71, 0.67, 0.76, 0.72, 0.64, 0.68, 0.62,
        0.57, 0.64, 0.64, 0.71, 0.71, 0.71, 0.8, 0.71, 0.71, 0.67
      ),
      times = c(
        0, 4.817, 13.033, 38.517, 50.3, 63.367, 85.183, 111.933, 134.683,
        146.333, 158.017, 176.8, 182.983, 189.517, 195.85, 201.2, 206.417,
        215.867, 218.917, 224.217, 229.867
      ) + 1.283,
      implementation = "interpolate"
    )
  )
  res <- create_event_table(
    regimen = reg,
    t_max = 185.266, t_tte = NULL, t_init = 1.283,
    t_obs = c(-1.283, 69.883, 85.183, 134.683, 182.983),
    p = list(), covariates = covs,
    model = mod_2cmt_iv,
    obs_type = rep(1, 5)
  )
  expect_equal(sum(is.na(res)), 0)
  expect_equal(dim(res), c(36, 13))
})
