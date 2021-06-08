test_that("merge regimens correctly", {
  reg1 <- new_regimen(amt = 1000, times = c(0, 24), type = "infusion", t_inf = 1)
  reg2 <- new_regimen(amt = 500, times = c(12, 36), type = "oral")
  reg3 <- merge_regimen(regimens = list(reg1, reg2))
  expect_equal(reg3$dose_times, c(0, 12, 24, 36))
  expect_equal(reg3$type, c("infusion", "oral", "infusion", "oral"))
  expect_equal(reg3$dose_amts, c(1000, 500, 1000, 500))
  expect_equal(reg3$t_inf, c(1, 0, 1, 0))
})
