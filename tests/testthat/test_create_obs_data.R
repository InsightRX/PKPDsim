test_that("create_obs_data returns correct format", {
  ode_data <- data.frame(
    time = c(0, 3, 6, 8, 12, 12),
    y.1 = c(100, 54.9, 30.1, 20.2, 9.1, 109.1),
    y.2 = c(0, 4.5, 7, 8, 9.1, 9.1),
    obs_type = c(0, 1, 1, 1, 0, 0),
    obs = c(100, 54.9, 30.1, 20.2, 9.1, 109.1)
  )
  obs_attr <- list(cmt = 1, scale = 1)
  dat <- create_obs_data(ode_data, obs_attr, id = 1)

  expect_equal(nrow(dat), nrow(ode_data))
  expect_equal(unique(dat$comp), "obs")
  expect_equal(dat$y, c(ode_data$obs))
})

test_that("create_obs_data returns correct format (multi-compartment obs)", {
  ode_data <- data.frame(
    time = c(0, 1, 2, 3, 4, 5),
    y.1 = c(100, 60.6, 36.7, 22.3, 13.5,8.2),
    y.2 = c(0, 37.2, 56.3, 64.7, 66.8, 65.5),
    obs_type = c(1L, 1L, 1L, 1L, 1L, 1L),
    obs1 = c(0, 37.2, 56.3, 64.7, 66.8, 65.5),
    obs2 = c(0, 0.7, 1.1, 1.2, 1.3, 1.3)
  )
  obs_attr <- list(cmt = c(2, 2), scale = c("1", "V"), label = c("abs", "conc"))
  dat <- create_obs_data(ode_data, obs_attr, id = 1)

  expect_equal(nrow(dat), 2 * nrow(ode_data))
  expect_equal(unique(dat$comp), c("abs", "conc"))
  expect_equal(dat$y, c(ode_data$obs1, ode_data$obs2))
})
