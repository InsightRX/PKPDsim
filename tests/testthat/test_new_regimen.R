test_that("Basic regimen creation working", {
  reg <- new_regimen(amt=100, n=4, interval=4)
  expect_true(all(c("regimen", "list") %in% class(reg)))
})

test_that("Regimen type parsed correctly", {
  reg_oral <- new_regimen(amt = 100, n = 4, interval = 4, type = "oral")
  reg_bolus <- new_regimen(amt = 100, n=4, interval = 4, type = "bolus")
  reg_inf <- new_regimen(amt = 100, n = 4, interval = 4, type = "infusion")
  reg_mixed <- new_regimen(amt = 100, n = 3, interval = 4, type = c("bolus", "infusion", "oral"))

  expect_equal(reg_oral$type, rep("oral", 4))
  expect_equal(reg_bolus$type, rep("bolus", 4))
  expect_equal(reg_inf$type, rep("infusion", 4))
  expect_equal(reg_mixed$type, c("bolus", "infusion", "oral"))
})

test_that("Auto-detect infusion vs bolus", {
  expect_silent(reg1 <- new_regimen(amt = 100, n = 4, interval = 4, t_inf = 0))
  expect_warning(reg2 <- new_regimen(amt = 100, n = 4, interval = 4, t_inf = 1))
  expect_warning(reg3 <- new_regimen(amt = 100, n = 4, interval = 4, t_inf = c(0, 1, 0, 1)))

  expect_equal(reg1$type, rep("bolus", 4))
  expect_equal(reg2$type, rep("infusion", 4))
  expect_equal(reg3$type, c("bolus", "infusion", "bolus", "infusion"))
})

test_that("n = 0 doses does not create doses at negative times", {
  reg0 <- new_regimen(amt = 200, n = 0, interval = 24, t_inf = 2, type = "infusion")
  expect_false(min(reg0$dose_times) < 0)
})

test_that("Rate argument creates valid PKPD regimen", {
  reg1 <- new_regimen(
    amt = 100,
    times = c(0, 12, 24, 36, 48),
    type = "infusion",
    rate = c(1,2,3,4,5)
  )
  expect_equal(round(reg1$t_inf), c(100, 50, 33, 25, 20))

  reg2 <- new_regimen(
    amt = 100,
    times = c(0, 12, 24, 36, 48),
    type = "infusion",
    rate = c(5)
  )
  expect_equal(round(reg2$t_inf), rep(20, 5))
})

test_that("Doses < 0 set to 0", {
  expect_warning(
    tmp <- new_regimen(amt = c(-1, -2, 3, 4), times = c(0, 24, 48, 72), type = "infusion")
  )
  expect_true(all(tmp$dose_amts >= 0))
})
