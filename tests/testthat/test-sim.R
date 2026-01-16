# ---- Shared Setup ----
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

# ---- Event Table Tests ----
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

# ---- Bioavailability Tests ----
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

# ---- Covariate and t_init Tests ----
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

# ---- Steady State Covariate Tests ----
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

# ---- t_max Tests ----
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

# ---- sim_core Tests ----
test_that("sim core works", {
  reg <- new_regimen(amt = 100, n = 5, interval = 12, t_inf = 1, type = "infusion")
  par <- list(KA = 1, CL = 5, V = 50)

  ## have to be explicit about t_obs with sim_core!
  f1 <- function() {
    res <- sim(ode = mod_1cmt_oral, regimen = reg, parameters = par, only_obs = TRUE, t_obs=c(0:24))$y
    return(res)
  }

  f2 <- function() {
    obj <- sim(ode = mod_1cmt_oral, regimen = reg, parameters = par, only_obs = TRUE, t_obs=c(0:24), return_design=TRUE)
    sim_core(obj, ode = mod_1cmt_oral)$y
  }
  expect_equal(f1(), f2())
})

test_that("sim core works for absorption model with lagtime", {
  reg <- new_regimen(amt = 100, n = 5, interval = 12, t_inf = 1, type = "infusion")
  par <- list(CL = 5, V = 50, KA = 0.5, TLAG = 0.83)

  ## have to be explicit about t_obs with sim_core!
  f_ref <- function() {
    res <- sim(
      ode = mod_1cmt_oral_lagtime,
      regimen = reg,
      parameters = par,
      only_obs = TRUE,
      t_obs=c(0:24)
    )$y
    return(res)
  }
  f_core <- function() {
    obj <- sim(
      ode = mod_1cmt_oral_lagtime,
      regimen = reg,
      parameters = par,
      only_obs = TRUE,
      t_obs=c(0:24),
      return_design=TRUE
    )
    sim_core(
      obj,
      ode = mod_1cmt_oral_lagtime,
      lagtime = c(0.83, 0)
    )$y ## Lagtime parameter needed!
  }
  expect_equal(f_ref(), f_core())
})

# ---- Lagtime Tests ----
test_that("dose dump after lagtime in correct order in output data", {
  skip_on_cran()
  reg <- new_regimen(amt = 500, n = 4, interval = 12, type = 'oral')
  pars <- list(CL = 5, V = 50, KA = 0.5, TLAG = 0.83)
  dat <- sim_ode(
    ode = mod_1cmt_oral_lagtime,
    regimen = reg,
    parameters = pars,
    only_obs = FALSE
  )
  ## Change after RXR-2394: time of TLAG not in dataset anymore unless requested by user in t_obs
  ## Before change: expect_equal(round(dat[dat$t == 12.83 & dat$comp == 1,]$y, 1), c(1.2, 501.2))
  ## After change:
  expect_equal(nrow(dat[dat$t == 12.83,]), 0)
  ## When grid requested by user, lagtime should be visible
  dat <- sim_ode(
    ode = mod_1cmt_oral_lagtime,
    regimen = reg,
    parameters = pars,
    t_obs = seq(0, 1, 0.01),
    only_obs = TRUE
  )
  tmp <- dat[dat$t >= 0.82 & dat$t <= 0.85, ]
  expect_equal(tmp$t, c(0.82, 0.83, 0.84, 0.85))
  expect_equal(round(tmp$y, 2), c(0, 0, 0.05, 0.10))
})

# ---- Result Comparison Tests ----
# Analytic solution for 1-cmt oral
pk1cmt_oral_anal <- function(t, dose, KA, V, CL) {
  dose*KA/(V*(KA-CL/V))*(exp(-(CL/V) * t)-exp(-KA * t))
}

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

  pk1cmt_oral_code_res <- sim_ode(
    ode = mod_1cmt_oral_code,
    parameters = p,
    duplicate_t_obs = TRUE,
    regimen=regimen,
    t_obs=t_obs,
    int_step_size = 0.1,
    only_obs=TRUE
  )

  pk1cmt_oral_anal_res <- pk1cmt_oral_anal(t_obs, dose, p$KA, p$V, p$CL)
  expect_equal(round(pk1cmt_oral_lib$y, 3), round(pk1cmt_oral_anal_res, 3))
  expect_equal(round(pk1cmt_oral_code_res$y, 3), round(pk1cmt_oral_anal_res, 3))
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

# ---- Dose Compartment Tests ----
test_that("Dose is added to correct compartment: specified by model", {
  set.seed(90)
  p <- list(CL = 1, V  = 10, KA = 0.5, S2 = .1)
  r <- new_regimen(amt = 100, times = c(0), type = "infusion")
  dat <- sim_ode(
    ode = mod_dose_cmt_2,
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
    ode = mod_dose_cmt_2,
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
    ode = mod_dose_cmt_2,
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

# ---- Covariate Timing Tests ----
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
