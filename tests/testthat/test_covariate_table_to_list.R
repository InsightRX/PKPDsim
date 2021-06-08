cov_table <- data.frame(
  id = c(1, 1, 2, 3),
  WT = c(40, 45, 50, 60),
  SCR = c(50, 150, 90,110),
  t = c(0, 5, 0, 0)
)

test_that("expected list object returned", {
  test1 <- covariates_table_to_list(cov_table)
  expect_equal(length(test1), length(unique(cov_table$id)))
  expect_true(is.list(test1))
  expect_true("covariate" %in% class(test1[[3]][[1]]))
  expect_equal(test1[[1]]$WT$value, c(40, 45))
  expect_equal(test1[[1]]$SCR$implementation, "interpolate")
  expect_equal(test1[[2]]$WT$value, 50)
})

test_that("implementation specified per covariate", {
  test2 <- covariates_table_to_list(
    cov_table,
    list(
      SCR = "locf",
      WT = "interpolate"
    )
  )
  expect_equal(test2[[1]]$SCR$implementation, "locf")
  expect_equal(test2[[1]]$WT$implementation, "interpolate")
})


