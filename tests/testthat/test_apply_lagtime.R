reg1 <- new_regimen(
  amt = 1000,
  n = 12,
  interval = 12,
  type = "oral"
)

test_that("lagtime applied to regimens using parameters", {
  lag1 <- apply_lagtime(
    regimen = reg1,
    lagtime = "TLAG",
    parameters = list(CL = 5, V = 50, TLAG = .5)
  )
  expect_true("regimen" %in% class(lag1))
  expect_equal(lag1$dose_times, reg1$dose_times + 0.5)
})

test_that("lagtime applied to regimens using lagtime arg", {
  lag2 <- apply_lagtime(regimen = reg1, lagtime = .75)
  expect_true("regimen" %in% class(lag2))
  expect_equal(lag2$dose_times, reg1$dose_times + 0.75)
})

test_that("lagtime applied when multiple compartments and no compartment specified in regimen", {
  lag3 <- apply_lagtime(
    regimen = reg1,
    lagtime = c("TLAG", 0, 0),
    parameters = list(CL = 5, V = 50, TLAG = .5)
  )
  expect_true("regimen" %in% class(lag3))
  expect_equal(lag3$dose_times, reg1$dose_times + 0.5)
})

test_that("lagtime applied when multiple compartments and no compartment specified in regimen but cmt_mapping available (oral)", {
  lag4a <- apply_lagtime(
    regimen = reg1,
    lagtime = c("TLAG", 0, 0),
    parameters = list(CL = 5, V = 50, TLAG = .5),
    cmt_mapping = list(oral = 1, infusion = 2, bolus = 2)
  )
  expect_true("regimen" %in% class(lag4a))
  expect_equal(lag4a$dose_times, reg1$dose_times + 0.5)
})

test_that("lagtime applied when multiple compartments and no compartment specified in regimen but cmt_mapping available (infusion): dose times should not be altered", {
  reg2 <- new_regimen(
    amt = 1000,
    n = 12,
    interval = 12,
    type = "infusion"
  )
  lag4b <- apply_lagtime(
    regimen = reg2,
    lagtime = c("TLAG", 0, 0),
    parameters = list(CL = 5, V = 50, TLAG = .5),
    cmt_mapping = list(oral = 1, infusion = 2, bolus = 2)
  )
  expect_true("regimen" %in% class(lag4b))
  expect_equal(lag4b$dose_times, reg2$dose_times)
})
