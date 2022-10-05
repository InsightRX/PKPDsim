reg1 <- new_regimen(
  amt = c(100, 100, 100),
  t_inf = c(24, 24, 28),
  time = c(0, 24, 48),
  type = "infusion"
)
reg2 <- new_regimen(
  amt = c(200, 200, 200),
  t_inf = c(24,24,24),
  time = c(0, 24, 48),
  type = "infusion"
)

test_that("Early update of a regimen works", {
  res1 <- join_regimen(reg1, reg2, t_dose_update = 65)
  res2 <- join_regimen(reg1, reg2, interval = 15)
  expect_equal(res1$t_inf, c(24, 24, 17, 24, 24, 24))
  expect_equal(round(res1$dose_amts[3], 1), round(100 * 17 / 28, 1))
  expect_equal(res2$t_inf, c(24, 24, 15, 24, 24, 24))
  expect_equal(round(res2$dose_amts[3], 1), round(100 * 15 / 28, 1))
})

test_that("Gap in regimen is ok", {
  res3 <- join_regimen(reg1, reg2, t_dose_update = 100)
  expect_equal(res3$t_inf, c(24, 24, 28, 24, 24, 24))
  expect_equal(res3$dose_amts, c(100, 100, 100, 200, 200, 200))
})

test_that("Update from time = 0", {
  res4 <- join_regimen(reg1, reg2, t_dose_update = 0)
  expect_equal(res4$dose_amts,reg2$dose_amts)
})


test_that("Early update of a regimen works when mixed routes", {
  reg3 <- new_regimen(
    amt = c(100, 100, 100),
    t_inf = c(0, 0, 0),
    time = c(0, 24, 48),
    type = "oral"
  )
  res0 <- join_regimen(reg3, reg2, dose_update = 1)
  expect_equal(res0$type, rep("infusion", 3))

  res1 <- join_regimen(reg3, reg2, dose_update = 3)
  expect_equal(res1$type, c(rep("oral", 2), rep("infusion", 3)))

  res2 <- join_regimen(reg3, reg2, dose_update = 4, interval = 24)
  expect_equal(res2$type, c(rep("oral", 3), rep("infusion", 3)))
})

test_that("insufficient join timing throws errors", {
  expect_error(
    join_regimen(reg1, reg2)
  )
})

test_that("dose_update longer than reg 1 length without interval specified throws error", {
  expect_error(
    join_regimen(reg1, reg2, dose_update = 4)
  )
})

test_that("one null regimen returns the other", {
  expect_equal(join_regimen(NULL, reg2), reg2)
  expect_equal(join_regimen(reg1, NULL), reg1)
})

test_that("join regimen works when no t_inf specified, e.g. oral or sc dosing", {
  reg_i <- new_regimen(amt = 500, type = "sc", times = c(0, 2, 6)*24*7)
  reg_m <- new_regimen(amt = 400, type = "sc", n = 5, interval = 8*7*24)
  reg <- join_regimen(reg_i, reg_m, interval = 8*7*24)
  expect_equal(reg$dose_times, c(0, 336, 1008, 2352, 3696, 5040, 6384, 7728))
  expect_equal(reg$type, c("sc", "sc", "sc", "sc", "sc", "sc", "sc", "sc"))
  expect_equal(reg$dose_amts, c(500, 500, 500, 400, 400, 400, 400, 400))
})
