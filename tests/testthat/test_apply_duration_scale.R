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

test_that("infusion length scaled", {
  dur4 <- apply_duration_scale(
    regimen = reg_combo,
    duration_scale = c(1, 1.5, 2)
  )
  expect_equal(dur4$t_inf, c(1.5, 2, 0, 0))
})
