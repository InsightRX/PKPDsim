test_that("Shifting by 1 works", {
  keys <- c("oral", "infusion", "bolus")
  for(key in keys) {
    reg1 <- new_regimen(amt = 2000, interval = 24, n = 6, type = key, t_inf = 1)
    reg1_s1 <- shift_regimen(reg1)
    expect_true("regimen" %in% class(reg1_s1))
    expect_equal(length(reg1_s1$dose_amts), 5)
  }
})

test_that("Shifting by N works", {
  keys <- c("oral", "infusion", "bolus")
  for(key in keys) {
    reg2 <- new_regimen(amt = c(1:6), times = c(0:5) * 24, type = "infusion")
    reg2_s1 <- shift_regimen(reg2, n = 3)
    expect_true("regimen" %in% class(reg2_s1))
    expect_equal(length(reg2_s1$dose_amts), 3)
    expect_equal(length(reg2_s1$t_inf), 3)
    # amounts taken from end, not front
    expect_equal(reg2_s1$dose_amts, c(4, 5, 6))
  }
})

test_that("Shifting by N > length(regimen) produces NULL", {
  keys <- c("oral", "infusion", "bolus")
  for(key in keys) {
    reg2 <- new_regimen(amt = c(1:6), times = c(0:5) * 24, type = "infusion")
    reg2_s2 <- shift_regimen(reg2, n = 10)
    expect_null(reg2_s2)
  }
})
