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

test_that("Update from dose 0", {
  res4 <- join_regimen(reg1, reg2, t_dose_update = 0)
  expect_equal(res4$dose_amts,reg2$dose_amts)
})
