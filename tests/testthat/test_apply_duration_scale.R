reg_oral <- new_regimen(
  amt = 1000,
  n = 12,
  interval = 12,
  type = "oral"
)
reg_iv <- new_regimen(
  amt = 1000,
  n = 12,
  interval = 12,
  type = "infusion",
  t_inf = 1,
  cmt = c(1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2)
)

reg_combo <- new_regimen(
  amt = 1000,
  n = 4,
  interval = 12,
  type = c("drug1", "drug2", "oral", "oral"),
  cmt = c(2, 3, 1, 1),
  t_inf = c(1, 1, 0, 0)
)

## null / error checks
test_that("infusion length NULL warning", {
  expect_warning(
    dur0 <- apply_duration_scale(
      regimen = reg_oral
    )
  )
  expect_equal(dur0, reg_oral)
})
test_that("duration scale NULL warning", {
  expect_warning(
    dur1 <- apply_duration_scale(
      regimen = reg_iv
    )
  )
  expect_equal(dur1, reg_iv)
})

## Check functionality
test_that("infusion length scaled", {
  dur2 <- apply_duration_scale(
    regimen = reg_iv,
    duration_scale = 1.5
  )
  expect_equal(dur2$t_inf, rep(1.5, 12))
})

test_that("infusion length scaled using vector", {
  dur3 <- apply_duration_scale(
    regimen = reg_iv,
    duration_scale = c(1.5, 1.0) # scale oral "infusions", but not "iv"
  )
  expect_equal(dur3$t_inf, rep(c(1.5, 1.0), 6))
})

test_that("infusion length scaled using parameter", {
  dur4 <- apply_duration_scale(
    regimen = reg_iv,
    duration_scale = "SCALE",
    parameters = list(CL = 5, V = 50, SCALE = 1.6)
  )
  expect_equal(dur4$t_inf, rep(1.6, 12))
})

test_that("infusion length scaled, multiple cmts", {
  dur4 <- apply_duration_scale(
    regimen = reg_combo,
    duration_scale = c(1, 1.5, 2)
  )
  expect_equal(dur4$t_inf, c(1.5, 2, 0, 0))
})

test_that("infusion length scaled, multiple cmts, no cmt", {
  reg_combo$cmt <- NULL
  dur5 <- apply_duration_scale(
    regimen = reg_combo,
    duration_scale = "SCALE",
    parameters = list(CL = 5, V = 50, SCALE = 1.6),
    cmt_mapping = c("drug1" = 2, "drug2" = 3, "oral" = 1)
  )
  dur6 <- apply_duration_scale(
    regimen = reg_combo,
    duration_scale = c("SCALE1", "SCALE2", "SCALE3"),
    parameters = list(CL = 5, V = 50, SCALE1 = 1.6, SCALE2 = 1.7, SCALE3 = 1.8),
    cmt_mapping = c("drug1" = 2, "drug2" = 3, "oral" = 1)
  )
  expect_equal(dur5$t_inf, c(1.6, 1.6, 0, 0))
  expect_equal(dur6$t_inf, c(1.7, 1.8, 0, 0))
})

test_that("infusion length scaled, no cmt, no cmt_mapping", {
  reg_combo$cmt <- NULL
  dur7 <- apply_duration_scale(
    regimen = reg_combo,
    duration_scale = c("SCALE1", "SCALE2", "SCALE3"),
    parameters = list(CL = 5, V = 50, SCALE1 = 1.6, SCALE2 = 1.7, SCALE3 = 1.8),
    cmt_mapping = NULL
  )
  expect_equal(dur7$t_inf, c(1.6, 1.6, 0, 0))
})
