
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

test_that("Multiple obs_types does not add erroneuous infusion stop events", {
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
})
