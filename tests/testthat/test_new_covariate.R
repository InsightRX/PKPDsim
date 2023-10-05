test_that("negative times get removed", {
  cov1 <- new_covariate(
    value = c(1,2,3,4),
    times = c(-10, -5, 0.5, 3),
    implementation = "interpolate",
    remove_negative_times = TRUE
  )
  cov2 <- new_covariate(
    value = c(1,2,3,4),
    times = c(-10, -5, 0.5, 3),
    implementation = "locf",
    remove_negative_times = TRUE
  )
  cov3 <- new_covariate(
    value = c(1,2,3,4),
    times = c(-10, -5, 0.5, 3),
    implementation = "interpolate",
    remove_negative_times = FALSE
  )
  expect_message(expect_message(cov4 <- new_covariate(value = c(0.8, 1.1), times=c(-0.0167, 0.5))))
  expect_equal(round(cov1$value,1), c(2.9, 3, 4))
  expect_equal(round(cov2$value,1), c(2, 3, 4))
  expect_equal(round(cov3$value,1),c(1, 2, 3, 4))
  expect_equal(sum(is.na(cov4$value)), 0)
})

test_that("units respected when t<0 but t[1] > 0", {
  cov_unit1 <- new_covariate(
    value = c(20, 10, 10, 20),
    times = c(-3, 1, 5, 10),
    unit = c("lbs", "kg", "kg", "lbs")
  )
  expect_equal(cov_unit1$unit, c("lbs", "kg", "kg", "lbs"))

  expect_message(cov_unit2 <- new_covariate(
    value = c(20, 10, 10, 20),
    times = c(1, 3, 5, 10),
    unit = c("lbs", "kg", "kg", "lbs")
  ))
  expect_equal(cov_unit2$unit, c("lbs", "kg", "kg", "lbs"))
})

test_that("Joining works as expected", {
  expect_message(expect_message(
      tmp_join <- new_covariate(value = c(1,2,3,4,5), times=c(0,.3,.5,1.7,1.9))
  ))
  expect_equal(length(tmp_join$times), 2)
  expect_equal(length(tmp_join$value), 2)
})

test_that("Interpolated times are rounded properly", {
  expect_message(
    tmp_rnd <- new_covariate(
      value = c(1,2,3),
      times = c(0, 3.321252, 3.5111234),
      interpolation_join_limit = 1,
      round_times = 3
    )
  )
  expect_equal(tmp_rnd$times[2], 3.416)
})


test_that("LOCF correctly implements non-numeric covs at t = 0", {
  res1 <- new_covariate(
    value = c("a", "b"),
    times = c(-0.5, 48),
    implementation = "LOCF",
    remove_negative_times = TRUE
  )
  expect_equal(res1$value, c("a", "b"))
  expect_equal(res1$times, c(0, 48))
  res2 <- new_covariate(
    value = c("a", "b"),
    times = c(-0.5, 48),
    implementation = "LOCF",
    remove_negative_times = FALSE
  )
  expect_equal(res2$value, c("a", "b"))
  expect_equal(res2$times, c(-0.5, 48))
})

test_that("implementation must be either locf or interpolate", {
  expect_error(new_covariate(value = 1, implementation = "foo"))
  # not case-sensitive:
  expect_error(new_covariate(value = 1, implementation = "LOCF"), NA)
  expect_equal(
    new_covariate(value = 1, implementation = "LOCF"),
    new_covariate(value = 1, implementation = "locf")
  )
})

test_that("character/factor values are implemented via locf", {
  v <- factor("a", levels = c("a", "b"))
  expect_message(
    new_covariate(value = "A", implementation = "interpolate", verbose = TRUE),
    "Non-numeric values cannot be interpolated. Switching to 'locf'."
  )
  expect_message(
    new_covariate(value = v, implementation = "interpolate", verbose = TRUE),
    "Non-numeric values cannot be interpolated. Switching to 'locf'."
  )
  res1 <- new_covariate(
    value = c("a", "b"),
    times = c(-0.5, 48),
    implementation = "interpolate",
    verbose = FALSE
  )
  res2 <- new_covariate(
    value = factor(c("a", "b"), levels = c("a", "b")),
    times = c(-0.5, 48),
    implementation = "interpolate",
    verbose = FALSE
  )
  res3 <- new_covariate(
    value = c("a", "b"),
    times = c(-0.5, 48),
    implementation = "LOCF"
  )

  expect_equal(res1, res3)
  expect_equal(res2, res3)
  expect_equal(res1$implementation, "locf")
})
