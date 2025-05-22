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
    data.frame(
      t = c(0, 2, 12, 14, 24, 26, 48, 48),
      dose = c(100, 0, 100, 0, 100, 0, 0, 0),
      type = c(1, 1, 1, 1, 1, 1, 0, 0),
      dum = c(0, 1, 0, 1, 0, 1, 0, 0),
      dose_cmt = c(1, 1, 1, 1, 1, 1, 0, 0),
      t_inf = c(2, 0, 2, 0, 2, 0, 0, 0),
      evid = c(1, 2, 1, 2, 1, 2, 0, 0),
      bioav = c(1, 0, 1, 0, 1, 0, 0, 0),
      rate = c(50,-50, 50,-50, 50,-50, 0, 0),
      obs_type = c(0, 1, 0, 0, 0, 0, 1, 1)
    )
  )
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
    structure(
      list(
        t = c(0, 0, 2, 12, 14, 24, 24, 26, 48),
        dose = c(0,  100, 0, 100, 0, 0, 100, 0, 0),
        type = c(0, 1, 1, 1, 1, 0, 1, 1, 0),
        dum = c(0, 0, 1, 0, 1, 0, 0, 1, 0),
        dose_cmt = c(0, 1, 1, 1, 1, 0, 1, 1, 0),
        t_inf = c(0, 2, 0, 2, 0, 0, 2, 0, 0),
        evid = c(2, 1, 2, 1, 2, 2, 1, 2, 0),
        bioav = c(1, 1, 0, 1, 0, 1, 1, 0, 0),
        rate = c(0, 50,-50, 50,-50, 0, 50,-50, 0),
        cov_CRCL = c(70, 70, 70, 70, 70, 80, 80, 80, 80),
        cov_t_CRCL = c(0, 0, 0, 0, 0, 24, 24, 24, 24),
        gradients_CRCL = c(0.416666666666667, 0.416666666666667, 0.416666666666667, 0.416666666666667, 0.416666666666667, 0, 0, 0, 0),
        cov_WT = c(70, 70, 70, 70, 70, 70, 70, 70, 70),
        cov_t_WT = c(0, 0, 0, 0, 0, 0, 0, 0, 0),
        gradients_WT = c(0, 0, 0, 0, 0, 0, 0, 0, 0),
        obs_type = c(0, 0, 1, 0, 0, 0, 0, 0, 1)
      ),
      row.names = c(2L, 1L, 3L, 4L, 5L,
                    6L, 7L, 8L, 9L),
      class = "data.frame"
    )
  )
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
