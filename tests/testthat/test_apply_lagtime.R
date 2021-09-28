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
