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


