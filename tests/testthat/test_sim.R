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

describe("IOV", {

  reg_iov <- new_regimen(
    amt = 100,
    interval = 24,
    n = 5,
    type = "infusion"
  )
  iov_var <- 0.3 ^ 2 # 30% IOV

  test_that("Throws error when `iov_bins` supplied but not present in model", {
    expect_error({
      sim(
        ode = pk_iov_none,
        parameters = pars_iov_no_iov,
        regimen = reg_iov,
        omega = c(
          0.3 # IIV in CL
        ),
        n = 1,
        iov_bins = c(0, 24, 48, 72, 999), # !!
        omega_type = "normal",
        only_obs = TRUE,
        output_include = list(parameters = TRUE, variables = TRUE)
      )
    }, "No IOV implemented for this model")
  })

  test_that("Throws error when number of `iov_bins` is higher than allowed for model", {
    expect_error({
      sim(
        ode = pk_iov,
        parameters = pars_iov,
        regimen = reg_iov,
        omega = c(
          iov_var, # IOV in CL
          0, iov_var,
          0, 0, iov_var,
          0, 0, 0, iov_var,
          0, 0, 0, 0, 0.3 # IIV in CL
        ),
        n = 1,
        iov_bins = c(0, 24, 48, 72, 999), # one bin too many
        omega_type = "normal",
        only_obs = TRUE,
        output_include = list(parameters = TRUE, variables = TRUE)
      )
    }, "Number of allowed IOV bins for model is lower")
  })

  test_that("Throws warning when number of `iov_bins` is lower than allowed for model", {
    expect_warning({
      sim(
        ode = pk_iov,
        parameters = pars_iov,
        regimen = reg_iov,
        omega = c(
          iov_var, # IOV in CL
          0, iov_var,
          0, 0, iov_var,
          0, 0, 0, iov_var,
          0, 0, 0, 0, 0.3 # IIV in CL
        ),
        n = 1,
        iov_bins = c(0, 24, 999), # one bin too few
        omega_type = "normal",
        only_obs = TRUE,
        output_include = list(parameters = TRUE, variables = TRUE)
      )}, "Number of allowed IOV bins for model is higher"
    )
  })

  test_that("IOV is added to parameters", {
    skip_on_cran()
    set.seed(32)

    dat <- sim(
      ode = pk_iov,
      parameters = pars_iov,
      regimen = reg_iov,
      omega = c(
        iov_var, # IOV in CL
        0, iov_var,
        0, 0, iov_var,
        0, 0, 0, iov_var,
        0, 0, 0, 0, 0.3 # IIV in CL
      ),
      n = 5,
      iov_bins = c(0, 24, 48, 72),
      omega_type = "normal",
      only_obs = TRUE,
      output_include = list(parameters = TRUE, variables = TRUE)
    )

    expect_equal(
      signif(dat$kappa_CL[dat$t == 12], 4),
      signif(dat$kappa_CL_1[dat$t == 1], 4)
    )
    expect_equal(
      signif(dat$kappa_CL[dat$t == 36], 4),
      signif(dat$kappa_CL_2[dat$t == 1], 4)
    )
    expect_equal(
      signif(dat$kappa_CL[dat$t == 48], 4),
      signif(dat$kappa_CL_3[dat$t == 1], 4)
    )
    expect_equal(
      signif(exp(dat$kappa_CL + dat$eta_CL) * dat$CL, 4),
      signif(dat$CL_iov, 4)
    )
  })
  
  test_that("error is not invoked when using parameters_table", {
    parameters_table <- data.frame(
      CL = runif(10), V = runif(10), KA = runif(10), eta_CL = runif(10),
      kappa_CL_1 = 0, kappa_CL_2 = 0, kappa_CL_3 = 0, kappa_CL_4 = 0
    )

    # specifying both parameters_table but for a model with IOV should not fail!
    expect_silent(
      dat <- sim(
        ode = pk_iov,
        parameters_table = parameters_table,
        regimen = reg_iov,
        omega = c(
          iov_var, # IOV in CL
          0, iov_var,
          0, 0, iov_var,
          0, 0, 0, iov_var,
          0, 0, 0, 0, 0.3 # IIV in CL
        ),
        n = 5,
        iov_bins = c(0, 24, 48, 72),
        omega_type = "normal",
        only_obs = TRUE,
        output_include = list(parameters = TRUE, variables = TRUE)
      )
    )

    # specifying both parameters and parameters_table should fail
    expect_error(
      dat <- sim(
        ode = pk_iov,
        parameters = pars_iov,
        parameters_table = parameters_table,
        regimen = reg_iov,
        omega = c(
          iov_var, # IOV in CL
          0, iov_var,
          0, 0, iov_var,
          0, 0, 0, iov_var,
          0, 0, 0, 0, 0.3 # IIV in CL
        ),
        n = 5,
        iov_bins = c(0, 24, 48, 72),
        omega_type = "normal",
        only_obs = TRUE,
        output_include = list(parameters = TRUE, variables = TRUE)
      )
    )
  })
})

describe("Compare results from sims with references", {

  # Uses models defined in setup.R:
  # - mod_1cmt_oral
  # - mod_1cmt_iv
  # - dose_in_cmt_2

  pk1cmt_oral_anal = function(t, dose, KA, V, CL) {
    dose*KA/(V*(KA-CL/V))*(exp(-(CL/V) * t)-exp(-KA * t))
  }

  pk1cmt_oral_code <- new_ode_model(
    code = "dAdt[1] = -KA*A[1]; dAdt[2] = KA*A[1] - (CL/V)*A[2];",
    obs = list(cmt = 2, scale = "V")
  )

  test_that("Model from library and custom C++ and code matches analytic solution", {
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
    skip_on_cran()
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

})

describe("Simulate with multiple observation types", {

  # Uses model defined in setup.R:
  # - pk_multi_obs
  # - vars_multi_obs

  regimen_multi_obs <- new_regimen(amt = 100, interval = 12, n = 5, type="infusion", t_inf = 1)
  parameters_multi_obs <- list("CL" = 15, "V" = 150)
  omega_multi_obs <- PKPDsim::cv_to_omega(list("CL" = 0.2, "V" = 0.2), parameters_multi_obs[1:2])

  test_that("obs types are output by `sim`", {
    obs_type <- c(1,2,1,3,1)
    data <- sim(
      ode = pk_multi_obs,
      parameters = list(CL = 20, V = 200),
      regimen = regimen_multi_obs,
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
      ode = pk_multi_obs,
      parameters = list(CL = 20, V = 200),
      regimen = regimen_multi_obs,
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
      ode = pk_multi_obs,
      parameters = list(CL = 20, V = 200),
      regimen = regimen_multi_obs,
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
      ode = pk_multi_obs,
      parameters = list(CL = 20, V = 200),
      regimen = regimen_multi_obs,
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
      ode = pk_multi_obs,
      parameters = parameters_multi_obs,
      n_ind = 1,
      regimen = regimen_multi_obs,
      only_obs = TRUE,
      t_obs = c(2,4,6,8),
      seed = 123456,
      res_var = ruv_single
    )

    ## specified as multi, but obs_type is all 1, so should
    ## give same results as s_single
    s_multi1 <- sim(
      ode = pk_multi_obs,
      parameters = parameters_multi_obs,
      n_ind = 1,
      regimen = regimen_multi_obs,
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
      ode = pk_multi_obs,
      parameters = parameters_multi_obs,
      n_ind = 1,
      regimen = regimen_multi_obs,
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
      pk_multi_obs,
      parameters = parameters_multi_obs,
      regimen = regimen_multi_obs,
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

})

describe("Multiple regimens", {
  test_that("multiple regimens for multiple individuals can be simulated", {

    # Uses a model defined in `setup.R`
    cov_table <- data.frame(WT = rnorm(10, 70, 5))
    multi_regs <- list()
    for(i in seq(cov_table$WT)) {
      multi_regs[[i]] <- new_regimen(amt = 10 * cov_table$WT[i], interval = 12, type = "infusion")
    }
    class(multi_regs) <- "regimen_multiple"

    par <- list(CL = 5, V = 50)
    reg <- new_regimen(amt = 2000, interval = 24, type = "infusion")
    covariates = list(WT = new_covariate(1))
    res <- sim_ode(
      ode = mod_1cmt_iv,
      parameters = par,
      covariates = covariates,
      regimen = multi_regs,
      only_obs = TRUE
    )
    expect_equal(length(unique(res$id)),10)
    expect_equal(sum(is.na(res$y)), 0)
    # IDs ordered correctly:
    expect_true(length(unique(res$id)) == 10 && all(diff(res$id) >= 0))
  })

})


describe("Mixture models", {
  # Uses model and covariates defined in setup.R:
  # - mod_mixture
  # - covs_mixture

  par_mixture <- list(CL = 3, V = 50)
  reg_mixture <- new_regimen(amt = 250, n = 5, interval = 6, type = 'infusion', t_inf = 1)
  t_obs_mixture <- seq(0, 36, 4)

  test_that("mixture model works properly for single patient", {
    res0 <- sim_ode(mod_mixture, parameters = par_mixture, regimen = reg_mixture, covariates = covs_mixture, t_obs = t_obs_mixture, only_obs=T) # mixture_group not supplied
    res1 <- sim(mod_mixture, parameters = par_mixture, regimen = reg_mixture, t_obs = t_obs_mixture, covariates = covs_mixture, mixture_group = 1, only_obs=T)
    res2 <- sim(mod_mixture, parameters = par_mixture, regimen = reg_mixture, t_obs = t_obs_mixture, covariates = covs_mixture, mixture_group = 2, only_obs=T)
    expect_equal(round(res0[res0$t == 24,]$y, 2), 9.07) # should use whatever is in `parameters`
    expect_equal(round(res1[res1$t == 24,]$y, 2), 5.82)
    expect_equal(round(res2[res2$t == 24,]$y, 2), 1.15)
  })

  test_that("mixture model works properly when vectorized (using parameters_table)", {
    partab <- data.frame(CL = rep(0, 6), V = rep(50, 6))
    suppressMessages({
      expect_error(sim_ode(mod_mixture, parameters_table = partab, regimen = reg_mixture, t_obs = t_obs_mixture, covariates = covs_mixture, mixture_group = 1, only_obs=T))
      res1 <- sim(mod_mixture, parameters_table = partab, regimen = reg_mixture, t_obs = t_obs_mixture, covariates = covs_mixture, mixture_group = rep(1, 6), only_obs=T)
      res2 <- sim(mod_mixture, parameters_table = partab, regimen = reg_mixture, t_obs = t_obs_mixture, covariates = covs_mixture, mixture_group = rep(c(1,2), 3), only_obs=T, output_include = list(parameters = TRUE))
    })
    expect_equal(round(res1[res1$t == 24,]$y, 2), rep(5.82, 6))
    expect_equal(round(res2[res2$t == 24,]$y, 2), rep(c(5.82, 1.15), 3))
    expect_equal(res2[res2$id == 1,]$CL[1], 5)
    expect_equal(res2[res2$id == 2,]$CL[1], 15)
    expect_equal(res2[res2$id == 3,]$CL[1], 5)
  })

  test_that("mixture model works properly when vectorized (using covariates_table)", {
    covtab <- data.frame(ID = 1:8, WT = rep(seq(40, 130, 30), 2))
    suppressMessages({
      expect_error(sim(mod_mixture, parameters = par_mixture, covariates_table = covtab, regimen = reg_mixture, t_obs = t_obs_mixture, mixture_group = 1, only_obs=T))
      res <- sim(mod_mixture, parameters = par_mixture, covariates_table = covtab, regimen = reg_mixture, t_obs = t_obs_mixture, mixture_group = rep(c(1, 2), each=4), only_obs=T)
    })
    expect_equal(round(res[res$t == 24,]$y, 2), c(9.39, 5.82, 3.83, 2.65, 2.99, 1.15, 0.52, 0.25))
  })

})

describe("Compartment mapping", {

  pk1cmt_oral_cmt_mapping <- new_ode_model(
    code = "dAdt[1] = -KA*A[1]; dAdt[2] = KA*A[1] - (CL/V)*A[2];",
    obs = list(cmt = 2, scale = "V"),
    cmt_mapping = list(oral = 1, infusion = 2, bolus = 2)
  )

  test_that("Compartment mapping is added to attributes", {
    expect_equal(attr(pk1cmt_oral_cmt_mapping, "cmt_mapping")[["oral"]], 1)
    expect_equal(attr(pk1cmt_oral_cmt_mapping, "cmt_mapping")[["infusion"]], 2)
  })

  test_that("Admin route is interpreted and simulated correctly", {
    regimen <- new_regimen(
      amt = c(100, 100, 100, 100),
      times = c(0, 12, 24, 36),
      type = c("oral", "oral", "infusion", "infusion"),
      t_inf = c(0, 0, 1, 1)
    )
    p <- list(KA = 1, CL = 5, V = 50)
    res <-  sim_ode(
      ode = pk1cmt_oral_cmt_mapping,
      parameters = p,
      regimen = regimen,
      only_obs = FALSE
    )
    expect_equal(round(res$y[res$comp == 1 & res$t == 25], 4), 2e-04)
    expect_true(res$y[res$comp == 2 & res$t == 25] >= 100)
  })

  test_that("multiple scaling types on one compartment works", {
    skip_on_cran()
    mod <- new_ode_model(
      code = "
        dAdt[1] = -KA * A[1];
        dAdt[2] = -(CL/V) * A[2] + KA*A[1];
      ",
      obs = list(
        cmt = c(2, 2),
        scale = c(1, "V"),
        label = c("abs", "conc")
      ),
      cpp_show_code = FALSE
    )
    par <- list(CL = 5, V = 50, KA = .5)
    reg <- new_regimen(amt = 100, n = 5, interval = 12)
    res <- sim_ode(
      ode = mod,
      parameters = par,
      regimen = reg,
      only_obs = TRUE
    )
    dat <- cbind(res[res$comp == "abs",]$y, res[res$comp == "conc",]$y)
    expect_true("PKPDsim" %in% class(mod))
    expect_equal(length(unique(res$comp)), 2)
    expect_equal(round(dat[,1],1), round(dat[,2]*par$V,1))
  })

})

describe("Model simulation with lagtime", {

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

})


describe("Models with t_init", {
  # Uses model defined in setup.R (conditional, NOT_CRAN only):
  # - mod_t_init

  skip_on_cran()

  # Test t_init functionality
  ## e.g. TDM before first dose:
  ## at t=-8, conc=10000
  ## Use this as true value in the simulations
  par_t_init <- list(CL = 7.67, V = 97.7, TDM_INIT = 500)
  reg_t_init <- new_regimen(amt = 100000, times=c(0, 24), type="bolus")
  s <- sim(
    ode = mod_t_init,
    parameters = par_t_init,
    n_ind = 1,
    regimen = reg_t_init,
    only_obs = TRUE,
    output_include = list(variables = TRUE),
    t_init = 10
  )

  test_that("TDM before first dose is considered a true initial value", {
    expect_equal(sum(is.na(s$y)), 0)
    expect_equal(s$y[1], 500)
    expect_equal(round(s$y[3]), 427)
    expect_equal(round(s$y[13]),1157)
  })

  test_that("Variables are set (also in first row) when TDM before first dose", {
    expect_equal(round(s$CONC[1:5], 1), c(500, 462.2, 427.3, 395.1, 365.3))
  })

})

describe("Time rounding issues", {

  # rounding time should not produce NAs in sim
  ## time-rounding bug 20170804

  test_that("No NAs related to rounding", {
    # Uses models defined in setup.R
    p <- list(CL = 5, V = 50, Q = 10, V2 = 150)
    r1 <- new_regimen(amt = 100, times = c(0, 24, 36), type = "infusion")
    dat1 <- sim_ode(
      ode = mod_2cmt_iv,
      parameters = p,
      regimen = r1,
      t_obs=seq(0, 150, length.out = 100)
    )
    expect_equal(sum(is.na(dat1$y)), 0)
  })

})

describe("Timevarying covariates are handled properly", {
  # Uses model defined in setup.R (conditional, NOT_CRAN only):
  # - mod_2cmt_timevar

  skip_on_cran()

  par_timevar <- list(CL = 3, V = 50, Q = 2.5, V2 = 70)
  reg_timevar <- new_regimen(
    amt = 250,
    n = 60,
    interval = 6,
    type = 'infusion',
    t_inf = 1
  )

  test_that("timevarying covariates handled", {
    # CLi changes by several orders of magnitude after
    # steady state is achieved, which should produce
    # a new steady state that is much lower
    covs <- list(
      CRCL = new_covariate(
        value = c(4.662744, 5.798767, 6.195943, 6.2, 600),
        times = c(0, 18, 29, 209, 210),
        implementation = "locf"
      )
    )
    t_obs <- seq(0, 360, 0.1)
    sim1 <- sim_ode(
      mod_2cmt_timevar,
      parameters = par_timevar,
      regimen = reg_timevar,
      covariates = covs,
      only_obs = TRUE,
      t_obs = t_obs,
      output_include = list(parameters = TRUE, covariates = TRUE)
    )
    expect_equal(sim1$CRCL[sim1$t == 209], 6.2)
    expect_equal(sim1$CRCL[sim1$t == 210], 600)
    expect_true(sim1$y[sim1$t == 35 * 6] > 10 * sim1$y[sim1$t == 60 * 6])
  })

  test_that("timevarying covariates are interpolated and affect PK", {
    covs_locf <- list(
      CRCL = new_covariate(
        times = c(0, 48, 96),
        value = c(3, 10, 3),
        implementation = "locf"
      )
    )
    covs_inter <- list(
      CRCL = new_covariate(
        times = c(0, 48, 96),
        value = c(3, 10, 3),
        implementation = "interpolate"
      )
    )
    t_obs <- seq(0, 120, 2)
    sim2_inter <- sim_ode(
      mod_2cmt_timevar,
      parameters = par_timevar,
      regimen = reg_timevar,
      covariates = covs_inter,
      only_obs = TRUE,
      t_obs = t_obs,
      output_include = list(parameters = TRUE, covariates = TRUE, variables = TRUE)
    )
    sim2_locf <- sim_ode(
      mod_2cmt_timevar,
      parameters = par_timevar,
      regimen = reg_timevar,
      covariates = covs_locf,
      only_obs = TRUE,
      t_obs = t_obs,
      output_include = list(parameters = TRUE, covariates = TRUE, variables = TRUE)
    )

    ## Check covariate changing for inter, but not for locf
    expect_equal(
      round(sim2_inter$CRCL, 3)[1:10],
      c(3, 3.292, 3.583, 3.875, 4.167, 4.458, 4.75, 5.042, 5.333, 5.625)
    )
    expect_equal(
      round(sim2_locf$CRCL, 3)[1:10],
      rep(3, 10)
    )

    ## Check interpolated covariates actually affect PK parameters
    expect_equal(
      round(sim2_inter$CLi, 3)[1:10],
      c(6, 6.292, 6.583, 6.875, 7.167, 7.458, 7.75, 8.042, 8.333, 8.625)
    )
    expect_equal(
      round(sim2_locf$CLi, 3)[1:10],
      rep(6, 10)
    )

    ## Check interpolated covariates actually affect simulated conc
    expect_equal(
      round(sim2_inter$y, 3)[1:10],
      c(0, 0.32, 0.611, 0.788, 1.205, 1.531, 1.708, 2.098, 2.379, 2.503)
    )
    expect_equal(
      round(sim2_locf$y, 3)[1:10],
      c(0, 0.321, 0.617, 0.805, 1.239, 1.598, 1.813, 2.251, 2.598, 2.792)
    )

    ## Visual check:
    # ggplot(sim2_inter, aes(x = t, y = y)) +
    #   geom_line() +
    #   geom_line(data = sim2_locf, colour = "blue")

  })

})