test_that("pop regimen without n", {
  keys <- c("oral", "infusion", "bolus")
  for(key in keys){
    reg1 <- new_regimen(amt = 2000, interval = 24, n = 6, type = key, t_inf = 1)
    reg1_s1 <- pop_regimen(reg1)

    expect_true("regimen" %in% class(reg1_s1))
    expect_equal(length(reg1_s1$dose_amts), 5)
  }
})

test_that("pop regimen with n specified", {
  keys <- c("oral", "infusion", "bolus")
  for(key in keys){
    reg1 <- new_regimen(amt = 2000, interval = 24, n = 6, type = key, t_inf = 1)
    reg1_s1 <- pop_regimen(reg1, 4)

    expect_true("regimen" %in% class(reg1_s1))
    expect_equal(length(reg1_s1$dose_amts), 2)
    expect_equal(length(reg1_s1$t_inf), 2)
  }
})

test_that("Amounts are taken from end and not start", {
  reg2 <- new_regimen(amt = c(1:6), times = c(0:5) * 24, type = "infusion")
  reg2_s1 <- pop_regimen(reg2, n = 3)
  expect_true("regimen" %in% class(reg2_s1))
  expect_equal(reg2_s1$dose_amts,  c(1, 2, 3))
})

test_that("returns NULL when no more doses", {
  reg2 <- new_regimen(amt = c(1:6), times = c(0:5) * 24, type = "infusion")
  reg2_s1 <- pop_regimen(reg2, n = 10)
  expect_null(reg2_s1)
})
