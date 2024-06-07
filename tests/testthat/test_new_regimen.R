test_that("Basic regimen creation working", {
  reg <- new_regimen(amt=100, n=4, interval=4)
  expect_true(all(c("regimen", "list") %in% class(reg)))
})

test_that("Regimen type parsed correctly", {
  reg_oral <- new_regimen(amt = 100, n = 4, interval = 4, type = "oral")
  reg_bolus <- new_regimen(amt = 100, n=4, interval = 4, type = "bolus")
  reg_inf <- new_regimen(amt = 100, n = 4, interval = 4, type = "infusion")
  reg_sc <- new_regimen(amt = 100, n = 4, interval = 4, type = "sc")
  reg_im <- new_regimen(amt = 100, n = 4, interval = 4, type = "im")
  reg_mixed <- new_regimen(amt = 100, n = 5, interval = 4, type = c("bolus", "infusion", "oral", "sc", "im"))

  expect_equal(reg_oral$type, rep("oral", 4))
  expect_equal(reg_bolus$type, rep("bolus", 4))
  expect_equal(reg_inf$type, rep("infusion", 4))
  expect_equal(reg_sc$type, rep("sc", 4))
  expect_equal(reg_im$type, rep("im", 4))
  expect_equal(reg_mixed$type, c("bolus", "infusion", "oral", "sc", "im"))
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

test_that("new_regimen can take arbitrary values for `type`", {
  reg <- new_regimen(100, times = 0, type = "pip")
  expect_equal(reg$type, "pip")
})

test_that("do not creat regimens of `type` 'covariate'", {
  expect_error(new_regimen(100, times = 0, type = "covariate"))
})

test_that("sc doses accept an infusion length argument'", {
  reg1 <- new_regimen(
    amt = 100,
    times = c(0, 12, 24, 36, 48),
    type = "sc",
    t_inf = 30/60
  )
  expect_equal(reg1$t_inf, rep(0.5,5))
})

test_that("t_inf imputed correctly", {
  reg1 <- new_regimen(
    amt = 100,
    times = c(0, 12, 24, 36, 48, 60, 72, 84),
    type = c("sc", "infusion", "im", "sc", "infusion", "im","bolus","oral")
  )
  reg2 <- new_regimen(
    amt = 100,
    times = c(0, 12, 24, 36, 48, 60, 72, 84),
    type = c("sc", "infusion", "im", "sc", "infusion", "im","bolus","oral"),
    t_inf = c(2/60, 2.5, 3/60, NA, NA, NA, NA, NA)
  )
  reg3 <- new_regimen(
    amt = 100,
    times = c(0, 12, 24, 36, 48, 60, 72, 84),
    type = c("sc", "infusion", "im", "sc", "infusion", "im","bolus","oral"),
    t_inf = numeric(0)
  )
  reg4 <- new_regimen(
    amt = 100,
    times = c(0, 12, 24, 36, 48, 60, 72, 84),
    type = c("sc", "infusion", "im", "sc", "infusion", "im","bolus","oral"),
    t_inf = NULL
  )
  reg5 <- new_regimen(
    amt = 100,
    times = c(0, 12, 24, 36),
    type = c("sc", "infusion", "im", "unknown_drug_type"),
    t_inf = c(2/60, 2.5, 3/60, NA)
  )
  expect_equal(reg1$t_inf, c(1/60, 1, 1/60, 1/60, 1, 1/60, 0, 0))
  expect_equal(reg2$t_inf, c(2/60, 2.5, 3/60, 1/60, 1, 1/60, 0, 0))
  expect_equal(reg3$t_inf, c(1/60, 1, 1/60, 1/60, 1, 1/60, 0, 0))
  expect_equal(reg4$t_inf, c(1/60, 1, 1/60, 1/60, 1, 1/60, 0, 0))
  expect_equal(reg5$t_inf, c(2/60, 2.5, 3/60, 1))
})

