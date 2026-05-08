

test_that("All 'type' should be handled as infusion unless bolus or oral", {
  reg1 <- new_regimen(
    amt = c(2400, 0, rep(1000, 5)), 
    times = seq(0, by = 24, length.out = 7),
    interval = 24, 
    type = "infusion_civ", 
    t_inf = c(24, 24, rep(2, 5))
  )
  reg2 <- reg1
  reg2$type <- rep("infusion", 7)
  reg3 <- reg1
  reg3$type <- rep("bolus", 7)

 tmp1 <- advan_create_data(
    parameters = list(CL = 1.72, V = 41.7, Q = 6.5, V2 = 38.4),
    regimen = reg1,
    t_obs = c(0, 120, 144)
  )
  tmp2 <- advan_create_data(
    parameters = list(CL = 1.72, V = 41.7, Q = 6.5, V2 = 38.4),
    regimen = reg2,
    t_obs = c(0, 120, 144)
  )
  tmp3 <- advan_create_data(
    parameters = list(CL = 1.72, V = 41.7, Q = 6.5, V2 = 38.4),
    regimen = reg3,
    t_obs = c(0, 120, 144)
  )
  expect_true(identical(tmp1, tmp2))
  expect_equal(
    tmp1$RATE,
    c(0, 100, 0, 500, 0, 500, 0, 500, 0, 0, 500, 0, 0, 500, 0)
  )

  expect_false(identical(reg3, reg2))
  expect_equal(
    tmp3$RATE,
    rep(0, 10) # treated as a bolus dose
  )
})