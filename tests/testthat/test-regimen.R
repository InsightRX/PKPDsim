# ---- new_regimen Tests ----
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
  reg_oral_susp <- new_regimen(amt = 1, n = 4, interval = 8, type = "oral_susp")

  expect_equal(reg_oral$type, rep("oral", 4))
  expect_equal(reg_bolus$type, rep("bolus", 4))
  expect_equal(reg_inf$type, rep("infusion", 4))
  expect_equal(reg_sc$type, rep("sc", 4))
  expect_equal(reg_im$type, rep("im", 4))
  expect_equal(reg_mixed$type, c("bolus", "infusion", "oral", "sc", "im"))
  expect_equal(reg_oral_susp$type, rep("oral_susp", 4))

  # oral-type regimens should have t_inf set to 0
  expect_equal(reg_oral$t_inf, rep(0, 4))
  expect_equal(reg_oral_susp$t_inf, rep(0, 4))
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

# ---- merge_regimen Tests ----
test_that("merge regimens correctly", {
  reg1 <- new_regimen(amt = 1000, times = c(0, 24), type = "infusion", t_inf = 1)
  reg2 <- new_regimen(amt = 500, times = c(12, 36), type = "oral")
  reg3 <- merge_regimen(regimens = list(reg1, reg2))
  expect_equal(reg3$dose_times, c(0, 12, 24, 36))
  expect_equal(reg3$type, c("infusion", "oral", "infusion", "oral"))
  expect_equal(reg3$dose_amts, c(1000, 500, 1000, 500))
  expect_equal(reg3$t_inf, c(1, 0, 1, 0))
  expect_null(reg3$cmt)
})

test_that("merge regimens correctly: two infusions", {
  reg1 <- new_regimen(amt = 1000, times = c(0, 24), type = "infusion_1", t_inf = 1)
  reg2 <- new_regimen(amt = 500, times = c(12, 36), type = "infusion_2", t_inf = 2)
  reg3 <- merge_regimen(regimens = list(reg1, reg2))
  expect_equal(reg3$dose_times, c(0, 12, 24, 36))
  expect_equal(reg3$type, c("infusion_1", "infusion_2", "infusion_1", "infusion_2"))
  expect_equal(reg3$dose_amts, c(1000, 500, 1000, 500))
  expect_equal(reg3$t_inf, c(1, 2, 1, 2))
  expect_null(reg3$cmt)
})

test_that("cmt info gets merged as well, when available", {
  reg1 <- new_regimen(amt = 1000, times = c(0, 24), type = "infusion", t_inf = 1, cmt = 2)
  reg2 <- new_regimen(amt = 500, times = c(12, 36), type = "oral", cmt = 1)
  reg3 <- merge_regimen(regimens = list(reg1, reg2))
  expect_equal(reg3$dose_times, c(0, 12, 24, 36))
  expect_equal(reg3$type, c("infusion", "oral", "infusion", "oral"))
  expect_equal(reg3$dose_amts, c(1000, 500, 1000, 500))
  expect_equal(reg3$t_inf, c(1, 0, 1, 0))
  expect_equal(reg3$cmt, c(2, 1, 2, 1))
})

# ---- join_regimen Tests ----
# Shared regimens for join tests
reg1_join <- new_regimen(
  amt = c(100, 100, 100),
  t_inf = c(24, 24, 28),
  time = c(0, 24, 48),
  type = "infusion"
)
reg2_join <- new_regimen(
  amt = c(200, 200, 200),
  t_inf = c(24,24,24),
  time = c(0, 24, 48),
  type = "infusion"
)

test_that("Early update of a regimen works", {
  res1 <- join_regimen(reg1_join, reg2_join, t_dose_update = 65)
  res2 <- join_regimen(reg1_join, reg2_join, interval = 15)
  expect_equal(res1$t_inf, c(24, 24, 17, 24, 24, 24))
  expect_equal(round(res1$dose_amts[3], 1), round(100 * 17 / 28, 1))
  expect_equal(res2$t_inf, c(24, 24, 15, 24, 24, 24))
  expect_equal(round(res2$dose_amts[3], 1), round(100 * 15 / 28, 1))
})

test_that("Gap in regimen is ok", {
  res3 <- join_regimen(reg1_join, reg2_join, t_dose_update = 100)
  expect_equal(res3$t_inf, c(24, 24, 28, 24, 24, 24))
  expect_equal(res3$dose_amts, c(100, 100, 100, 200, 200, 200))
})

test_that("Update from time = 0", {
  res4 <- join_regimen(reg1_join, reg2_join, t_dose_update = 0)
  expect_equal(res4$dose_amts, reg2_join$dose_amts)
})

test_that("Early update of a regimen works when mixed routes", {
  reg3 <- new_regimen(
    amt = c(100, 100, 100),
    t_inf = c(0, 0, 0),
    time = c(0, 24, 48),
    type = "oral"
  )
  res0 <- join_regimen(reg3, reg2_join, dose_update = 1)
  expect_equal(res0$type, rep("infusion", 3))

  res1 <- join_regimen(reg3, reg2_join, dose_update = 3)
  expect_equal(res1$type, c(rep("oral", 2), rep("infusion", 3)))

  res2 <- join_regimen(reg3, reg2_join, dose_update = 4, interval = 24)
  expect_equal(res2$type, c(rep("oral", 3), rep("infusion", 3)))
})

test_that("insufficient join timing throws errors", {
  expect_error(
    join_regimen(reg1_join, reg2_join)
  )
})

test_that("dose_update longer than reg 1 length without interval specified throws error", {
  expect_error(
    join_regimen(reg1_join, reg2_join, dose_update = 4)
  )
})

test_that("one null regimen returns the other", {
  expect_equal(join_regimen(NULL, reg2_join), reg2_join)
  expect_equal(join_regimen(reg1_join, NULL), reg1_join)
})

test_that("join regimen works when no t_inf specified, e.g. oral or sc dosing", {
  reg_i <- new_regimen(amt = 500, type = "sc", times = c(0, 2, 6)*24*7)
  reg_m <- new_regimen(amt = 400, type = "sc", n = 5, interval = 8*7*24)
  reg <- join_regimen(reg_i, reg_m, interval = 8*7*24)
  expect_equal(reg$dose_times, c(0, 336, 1008, 2352, 3696, 5040, 6384, 7728))
  expect_equal(reg$type, c("sc", "sc", "sc", "sc", "sc", "sc", "sc", "sc"))
  expect_equal(reg$dose_amts, c(500, 500, 500, 400, 400, 400, 400, 400))
})

test_that("join_regimen with t_dose_update maintains correct rate length", {
  reg1 <- new_regimen(
    amt = 1,
    times = c(0, 24),
    type = "oral"
  )
  reg2 <- new_regimen(
    amt = 2,
    time = c(0, 24, 48),
    type = "oral"
  )
  reg3 <- join_regimen(reg1, reg2, t_dose_update = 24)

  expect_equal(length(reg3$rate), length(reg3$dose_times))
  expect_equal(reg3$n, 4)
})

# ---- shift_regimen Tests ----
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
