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

## One compartment
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


## Two compartment
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



## Three compartment
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
