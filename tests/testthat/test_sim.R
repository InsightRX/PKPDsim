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

test_that("return_event_table=TRUE returns an appropriate event table", {
  evtab1 <- sim_ode(
    mod_1cmt_iv,
    parameters = par,
    regimen = reg,
    t_obs = t_obs,
    return_event_table = TRUE
  )
  expect_equal(
    evtab1,
    structure(list(
      t = c(0, 2, 2, 12, 14, 24, 26, 48, 48),
      dose = c(100, 0, 0, 100, 0, 100, 0, 0, 0),
      type = c(1, 0, 1, 1, 1, 1, 1, 0, 0),
      dum = c(0, 0, 1, 0, 1, 0, 1, 0, 0),
      dose_cmt = c(1, 0, 1, 1, 1, 1, 1, 0, 0),
      t_inf = c(2, 0, 0, 2, 0, 2, 0, 0, 0),
      evid = c(1, 0, 2, 1, 2, 1, 2, 0, 0),
      bioav = c(1, 0, 0, 1, 0, 1, 0, 0, 0),
      rate = c(50, 0, -50, 50, -50, 50, -50, 0, 0),
      obs_type = c(0, 1, 1, 0, 0, 0, 0, 1, 1)
    ),
    row.names = c(1L, 3L, 2L, 4L, 5L, 6L, 7L, 8L, 9L),
    class = "data.frame"
  ))
})

test_that("return_event_table=TRUE returns an appropriate event table with covariate", {
  covs <- list(CRCL = new_covariate(value = c(70, 80), t = c(0, 24)), WT = new_covariate(70))
  evtab2 <- sim_ode(
    mod_1cmt_iv,
    parameters = par,
    regimen = reg,
    t_obs = t_obs,
    covariates = covs,
    return_event_table = TRUE
  )
  expect_equal(
    evtab2,
    structure(list(
      t = c(0, 0, 2, 2, 12, 14, 24, 24, 26, 48),
      dose = c(0, 100, 0, 0, 100, 0, 0, 100, 0, 0),
      type = c(0, 1, 0, 1, 1, 1, 0, 1, 1, 0),
      dum = c(0, 0, 0, 1, 0, 1, 0, 0, 1, 0),
      dose_cmt = c(0, 1, 0, 1, 1, 1, 0, 1, 1, 0),
      t_inf = c(0, 2, 0, 0, 2, 0, 0, 2, 0, 0),
      evid = c(2, 1, 0, 2, 1, 2, 2, 1, 2, 0),
      bioav = c(1, 1, 0, 0, 1, 0, 1, 1, 0, 0),
      rate = c(0, 50, 0, -50, 50, -50, 0, 50, -50, 0),
      cov_CRCL = c(70, 70, 70, 70, 70, 70, 80, 80, 80, 80),
      cov_t_CRCL = c(0, 0, 0, 0, 0, 0, 24, 24, 24, 24),
      gradients_CRCL = c(0.416666666666667, 0.416666666666667, 0.416666666666667, 0.416666666666667, 0.416666666666667, 0.416666666666667, 0, 0, 0, 0),
      cov_WT = c(70, 70, 70, 70, 70, 70, 70, 70, 70, 70),
      cov_t_WT = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
      gradients_WT = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
      obs_type = c(0, 0, 1, 1, 0, 0, 0, 0, 0, 1)
    ),
    row.names = c(2L, 1L, 4L, 3L, 5L, 6L, 8L, 7L, 9L, 10L),
    class = "data.frame"
  ))
})

test_that("sim works properly for a model where bioavailability is dependent on dose", {
  skip_on_cran()
  reg <- new_regimen(amt = 1000, n = 4, interval = 12, type = 'oral')
  pars <- list(
    "TLAG" = 0.21,
    "KA" = 3.9,
    "CL" = 17.0,
    "V" = 68,
    "F1" = 1
  )
  mod <- new_ode_model( # simplified from de Winter / mmf with CSA
    code = "
    F1i = F1_avg \
    dAdt[0] = -KA*A[0] \
    dAdt[1] = +KA*A[0] - (CLi/Vi)*A[1]
    dAdt[2] = A[1]/Vi \
  ",
    pk_code = "
    TLAGi = TLAG \
    DS = prv_dose \
    if (DS < 250) { \
      DS = 250 \
    } \
    F1_avg = F1 * pow(DS/1000, -0.15) \
    KAi   = KA \
    CLi   = CL \
    Vi    = V
    ",
    lagtime = c("TLAG", 0, 0),
    obs = list(cmt = 2, scale = "V"),
    dose = list(cmt = 1, bioav = 1),
    declare_variables = c("CLi", "Vi", "KAi", "DS", "TLAGi", "F1_avg"),
    parameters = pars
  )
  dat <- sim_ode(
    ode = mod,
    regimen = reg,
    parameters = pars,
    output_include = list(variables = T, parameters = T),
    only_obs = FALSE
  )
  expect_true(all(round(dat$F1_avg, 5) == 1)) # should all be 1 from the first value, with no NAs
})

test_that("covariates and doses are shifted correctly when t_init != 0", {
  # example covs use both interpolation and locf methods.
  covs <- list(
    CR = new_covariate(
      value = c(0.5, 0.9),
      times = c(3, 30),
      implementation = "interpolate"
    ),
    CRRT = new_covariate(
      value = c(0, 1),
      times = c(0, 30),
      implementation = "locf"
    )
  )

  # use return_event_table = TRUE since we can just check that the event table
  # is correct, we don't need to simulate the whole ODE
  evtab1 <- sim_ode(
    mod_1cmt_iv,
    parameters = par,
    covariates = covs,
    regimen = reg,
    t_obs = t_obs,
    t_init = 48,
    return_event_table = TRUE
  )

  # the first observations for both covs should be the same as the initial
  # available observations, with no gradient:
  row1 <- evtab1[1,]
  expect_equal(row1$cov_CR, covs$CR$value[1])
  expect_equal(row1$cov_CRRT, covs$CRRT$value[1])
  expect_equal(row1$gradients_CR, 0)
  expect_equal(row1$gradients_CRRT, 0)

  # the event table should have rows for covariate changes at t = 3 + 48 and
  # t = 30 + 48
  CR_change <- evtab1[which(evtab1$gradients_CR > 0)[1], ]
  CRRT_change <- evtab1[which(evtab1$cov_CRRT > 0)[1], ]
  expect_equal(CR_change$t, 3 + 48)
  expect_equal(CRRT_change$t, 30 + 48)
  expect_equal(CR_change$evid, 2)
  expect_equal(CRRT_change$evid, 2)

  # the first dose value should be at t = 0 + 48
  first_dose <- evtab1[which(evtab1$evid == 1)[1], ]
  expect_equal(first_dose$t, 0 + 48)
  expect_equal(first_dose, evtab1[which(evtab1$dose > 0)[1], ])

  # also works fine when covs are NULL
  evtab2 <- sim_ode(
    mod_1cmt_iv,
    parameters = par,
    covariates = NULL,
    regimen = reg,
    t_obs = t_obs,
    t_init = 48,
    return_event_table = TRUE
  )
  # the first dose value should be at t = 0 + 48
  first_dose <- evtab2[which(evtab2$evid == 1)[1], ]
  expect_equal(first_dose$t, 0 + 48)
  expect_equal(first_dose, evtab2[which(evtab2$dose > 0)[1], ])
})


test_that("covariates_table and doses are shifted correctly when t_init != 0", {
  cov_table <- data.frame(
    id = c(1, 1),
    SCR = c(50, 150),
    t = c(3, 13)
  )

  # use return_event_table = TRUE since we can just check that the event table
  # is correct, we don't need to simulate the whole ODE
  attr( mod_1cmt_iv, "covariates") <- "SCR" # req'd to add cov to event table
  evtab1 <- suppressMessages(sim_ode(
    mod_1cmt_iv,
    parameters = par,
    covariates_table = cov_table,
    regimen = reg,
    t_obs = t_obs,
    t_init = 91,
    return_event_table = TRUE
  ))

  # the first observation should be the same as the initial available
  # observations, with no gradient:
  row1 <- evtab1[1,]
  expect_equal(row1$cov_SCR, cov_table$SCR[1])
  expect_equal(row1$gradients_SCR, 0)

  # the event table should have a row for covariate changes at t = 3 + 91
  SCR_change <- evtab1[which(evtab1$gradients_SCR > 0)[1], ]
  expect_equal(SCR_change$t, 3 + 91)
  expect_equal(SCR_change$evid, 2)

  # the first dose value should be at t = 0 + 91
  first_dose <- evtab1[which(evtab1$evid == 1)[1], ]
  expect_equal(first_dose$t, 0 + 91)
  expect_equal(first_dose, evtab1[which(evtab1$dose > 0)[1], ])
})


test_that("covariates are shifted correctly when t_ss != 0", {
  covs <- list(
    SCR = new_covariate(
      value = c(0.5, 0.9),
      times = c(3, 30),
      implementation = "interpolate"
    ),
    CRRT = new_covariate(
      value = c(0, 1),
      times = c(0, 30),
      implementation = "locf"
    )
  )

  reg <- new_regimen(
    amt = 1000, interval = 12, n = 6, t_inf = 1, type = "infusion",
    ss = TRUE
  )
  t_ss <- max(reg$ss_regimen$dose_times) + reg$interval # 120

  # use return_event_table = TRUE since we can just check that the event table
  # is correct, we don't need to simulate the whole ODE
  attr( mod_1cmt_iv, "covariates") <- "SCR" # req'd to add cov to event table
  evtab1 <- suppressMessages(sim(
    mod_1cmt_iv,
    parameters = par,
    covariates = covs,
    regimen = reg,
    t_obs = t_obs,
    t_init = 0,
    return_event_table = TRUE
  ))

  # the first observation should be the same as the initial available
  # observations, with no gradient:
  row1 <- evtab1[1,]
  expect_equal(row1$cov_SCR, covs$SCR$value[1])
  expect_equal(row1$gradients_SCR, 0)

  # the event table should have a row for covariate changes at t = 3 + 120
  SCR_change <- evtab1[which(evtab1$gradients_SCR > 0)[1], ]
  expect_equal(SCR_change$t, 3 + t_ss)
  expect_equal(SCR_change$evid, 2)

  # the first dose value should be at t = 0
  first_dose <- evtab1[which(evtab1$evid == 1)[1], ]
  expect_equal(first_dose$t, 0)
  expect_equal(first_dose, evtab1[which(evtab1$dose > 0)[1], ])
})

test_that("covariates_table is shifted correctly when steady state reg", {
  cov_table <- data.frame(
    id = c(1, 1),
    SCR = c(50, 150),
    t = c(3, 13)
  )
  reg <- new_regimen(
    amt = 1000, interval = 12, n = 6, t_inf = 1, type = "infusion",
    ss = TRUE
  )
  t_ss <- max(reg$ss_regimen$dose_times) + reg$interval

  # use return_event_table = TRUE since we can just check that the event table
  # is correct, we don't need to simulate the whole ODE
  attr( mod_1cmt_iv, "covariates") <- "SCR" # req'd to add cov to event table
  evtab1 <- suppressMessages(sim_ode(
    mod_1cmt_iv,
    parameters = par,
    covariates_table = cov_table,
    regimen = reg,
    t_obs = t_obs,
    t_init = 0,
    return_event_table = TRUE
  ))

  # the first observation should be the same as the initial available
  # observations, with no gradient:
  row1 <- evtab1[1,]
  expect_equal(row1$cov_SCR, cov_table$SCR[1])
  expect_equal(row1$gradients_SCR, 0)

  # the event table should have a row for covariate changes at t = 3 + 120
  SCR_change <- evtab1[which(evtab1$gradients_SCR > 0)[1], ]
  expect_equal(SCR_change$t, 3 + t_ss)
  expect_equal(SCR_change$evid, 2)

  # the first dose value should be at t = 0
  first_dose <- evtab1[which(evtab1$evid == 1)[1], ]
  expect_equal(first_dose$t, 0)
  expect_equal(first_dose, evtab1[which(evtab1$dose > 0)[1], ])
})

test_that("covariates are shifted correctly when t_ss != 0 & t_init != 0", {
  covs <- list(
    SCR = new_covariate(
      value = c(0.5, 0.9),
      times = c(3, 30),
      implementation = "interpolate"
    ),
    CRRT = new_covariate(
      value = c(0, 1),
      times = c(0, 30),
      implementation = "locf"
    )
  )

  reg <- new_regimen(
    amt = 1000, interval = 12, n = 6, t_inf = 1, type = "infusion",
    ss = TRUE
  )
  t_ss <- max(reg$ss_regimen$dose_times) + reg$interval # 120

  # use return_event_table = TRUE since we can just check that the event table
  # is correct, we don't need to simulate the whole ODE
  attr( mod_1cmt_iv, "covariates") <- "SCR" # req'd to add cov to event table
  evtab1 <- suppressMessages(sim(
    mod_1cmt_iv,
    parameters = par,
    covariates = covs,
    regimen = reg,
    t_obs = t_obs,
    t_init = 7,
    return_event_table = TRUE
  ))

  # the first observation should be the same as the initial available
  # observations, with no gradient:
  row1 <- evtab1[1,]
  expect_equal(row1$cov_SCR, covs$SCR$value[1])
  expect_equal(row1$gradients_SCR, 0)

  # the event table should have a row for covariate changes at t = 3 + 120
  SCR_change <- evtab1[which(evtab1$gradients_SCR > 0)[1], ]
  expect_equal(SCR_change$t, 3 + t_ss)
  expect_equal(SCR_change$evid, 2)

  # the first dose value should be at t = 0
  first_dose <- evtab1[which(evtab1$evid == 1)[1], ]
  expect_equal(first_dose$t, 0)
  expect_equal(first_dose, evtab1[which(evtab1$dose > 0)[1], ])

  # now try where t_init is very long ago, i.e., before start of ss (rare)
  evtab2 <- suppressMessages(sim(
    mod_1cmt_iv,
    parameters = par,
    covariates = covs,
    regimen = reg,
    t_obs = t_obs,
    t_init = 140,
    return_event_table = TRUE
  ))

  # the first observation should be the same as the initial available
  # observations, with no gradient:
  row1 <- evtab2[1,]
  expect_equal(row1$cov_SCR, covs$SCR$value[1])
  expect_equal(row1$gradients_SCR, 0)

  # the event table should have a row for covariate changes at t = 3 + 140
  SCR_change <- evtab2[which(evtab2$gradients_SCR > 0)[1], ]
  expect_equal(SCR_change$t, 3 + 140)
  expect_equal(SCR_change$evid, 2)

  # the first dose value should be at t = 140 - t_ss
  first_dose <- evtab2[which(evtab2$evid == 1)[1], ]
  expect_equal(first_dose$t, 140 - t_ss)
  expect_equal(first_dose, evtab2[which(evtab2$dose > 0)[1], ])
})

test_that("covariates_table shifted correctly when steady state reg + t_init", {
  cov_table <- data.frame(
    id = c(1, 1),
    SCR = c(50, 150),
    t = c(3, 13)
  )
  reg <- new_regimen(
    amt = 1000, interval = 12, n = 6, t_inf = 1, type = "infusion",
    ss = TRUE
  )
  t_ss <- max(reg$ss_regimen$dose_times) + reg$interval

  # use return_event_table = TRUE since we can just check that the event table
  # is correct, we don't need to simulate the whole ODE
  attr( mod_1cmt_iv, "covariates") <- "SCR" # req'd to add cov to event table
  evtab1 <- suppressMessages(sim_ode(
    mod_1cmt_iv,
    parameters = par,
    covariates_table = cov_table,
    regimen = reg,
    t_obs = t_obs,
    t_init = 7,
    return_event_table = TRUE
  ))

  # the first observation should be the same as the initial available
  # observations, with no gradient:
  row1 <- evtab1[1,]
  expect_equal(row1$cov_SCR, cov_table$SCR[1])
  expect_equal(row1$gradients_SCR, 0)

  # the event table should have a row for covariate changes at t = 3 + 120
  SCR_change <- evtab1[which(evtab1$gradients_SCR > 0)[1], ]
  expect_equal(SCR_change$t, 3 + t_ss)
  expect_equal(SCR_change$evid, 2)

  # the first dose value should be at t = 0
  first_dose <- evtab1[which(evtab1$evid == 1)[1], ]
  expect_equal(first_dose$t, 0)
  expect_equal(first_dose, evtab1[which(evtab1$dose > 0)[1], ])

  # now for t_init > t_ss
  evtab2 <- suppressMessages(sim_ode(
    mod_1cmt_iv,
    parameters = par,
    covariates_table = cov_table,
    regimen = reg,
    t_obs = t_obs,
    t_init = 140,
    return_event_table = TRUE
  ))
  # the first observation should be the same as the initial available
  # observations, with no gradient:
  row1 <- evtab2[1,]
  expect_equal(row1$cov_SCR, cov_table$SCR[1])
  expect_equal(row1$gradients_SCR, 0)

  # the event table should have a row for covariate changes at t = 3 + 140
  SCR_change <- evtab2[which(evtab2$gradients_SCR > 0)[1], ]
  expect_equal(SCR_change$t, 3 + 140)
  expect_equal(SCR_change$evid, 2)

  # the first dose value should be at t = 140 - t_ss
  first_dose <- evtab2[which(evtab2$evid == 1)[1], ]
  expect_equal(first_dose$t, 140 - t_ss)
  expect_equal(first_dose, evtab2[which(evtab2$dose > 0)[1], ])
})

test_that("times are recalculated correctly after steady-state regimen added", {
  covs <- list(
    SCR = new_covariate(
      value = c(0.5, 0.9),
      times = c(3, 30),
      implementation = "interpolate"
    ),
    CRRT = new_covariate(
      value = c(0, 1),
      times = c(0, 30),
      implementation = "locf"
    )
  )

  reg <- new_regimen(
    amt = 1000, interval = 12, n = 6, t_inf = 1, type = "infusion",
    ss = TRUE
  )
  t_obs <- c(0, 3, 24, 30, 48)
  attr( mod_1cmt_iv, "covariates") <- "SCR" # req'd to add cov to event table
  res <- suppressMessages(sim(
    mod_1cmt_iv,
    parameters = par,
    covariates = covs,
    regimen = reg,
    t_obs = t_obs,
    t_init = 0,
    only_obs = TRUE,
    output_include = list(covariates = TRUE)
  ))

  # we expect SCR before t = 3 to be 0.5, SCR after t = 30 to be 0.9, and
  # a gradient applied in between
  expect_equal(res$SCR, c(0.5, 0.5, 0.811111111111111, 0.9, 0.9))
  # t_obs passed as argument should match returned values
  expect_equal(res$t, t_obs)
  # we expect y to be consistent across the scenarios in this test as well
  expect_equal(res$y, c(9.06594585, 22.29872435, 9.06599650, 16.51932909, 9.06600109))

  # the above should also be true if t_init > steady state duration
  res <- suppressMessages(sim(
    mod_1cmt_iv,
    parameters = par,
    covariates = covs,
    regimen = reg,
    t_obs = t_obs,
    t_init = max(reg$ss_regimen$dose_times) + 24,
    only_obs = TRUE,
    output_include = list(covariates = TRUE)
  ))
  expect_equal(res$SCR, c(0.5, 0.5, 0.811111111111111, 0.9, 0.9))
  expect_equal(res$t, t_obs)
  expect_equal(res$y, c(9.06594585, 22.29872435, 9.06599650, 16.51932909, 9.06600109))

  # the above should also be true if t_init != 0 and there's an observation
  # before t == 0
  t_obs <- c(-15, 0, 3, 24, 30, 48)
  res <- suppressMessages(sim(
    mod_1cmt_iv,
    parameters = par,
    covariates = covs,
    regimen = reg,
    t_obs = t_obs,
    t_init = 15,
    only_obs = TRUE,
    output_include = list(covariates = TRUE)
  ))
  expect_equal(res$SCR, c(0.5, 0.5, 0.5, 0.811111111111111, 0.9, 0.9))
  expect_equal(res$t, t_obs)
  expect_equal(
    res$y, c(12.23757239, 9.06594585, 22.29872435, 9.06599650, 16.51932909, 9.06600109)
  )
})

test_that("t_max is shifted correctly when t_ss != 0", {
  reg <- new_regimen(
    amt = 1000, interval = 12, n = 6, t_inf = 1, type = "infusion",
    ss = TRUE
  )
  t_ss <- max(reg$ss_regimen$dose_times) + reg$interval # 120

  # check case where t_obs < t_max
  evtab1 <- suppressMessages(sim(
    mod_1cmt_iv,
    parameters = par,
    regimen = reg,
    t_obs = c(2, 48),
    t_init = 0,
    t_max = 50,
    return_event_table = TRUE
  ))
  expect_true(max(evtab1$t) <= 48 + t_ss)
  expect_equal(sum(is.na(evtab1)), 0)
  expect_equal(evtab1$t[evtab1$evid == 1], seq(0, 168, 12))

  # check case where t_max < t_obs
  evtab2 <- suppressMessages(sim(
    mod_1cmt_iv,
    parameters = par,
    regimen = reg,
    t_obs = c(2, 48),
    t_init = 0,
    t_max = 40, # before max t_obs
    return_event_table = TRUE
  ))
  expect_true(max(evtab2$t) <= 48 + t_ss) # t_max is max(t_obs)
  expect_equal(sum(is.na(evtab2)), 0)
  expect_equal(evtab2$t[evtab2$evid == 1], seq(0, 168, 12))

  # check case where no t_obs
  evtab3 <- suppressMessages(sim(
    mod_1cmt_iv,
    parameters = par,
    regimen = reg,
    t_obs = NULL,
    t_init = 0,
    t_max = 40,
    return_event_table = TRUE
  ))
  expect_equal(sum(is.na(evtab3)), 0)
})
