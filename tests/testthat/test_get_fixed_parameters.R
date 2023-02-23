test_that("get_fixed_parameters returns fixed params listed in model def", {
  def <- list(
    parameters = list(
      CL = 5.312,
      V = 42.52,
      V2 = 41.68,
      Q = 3.222
    ),
    fixed = "Q"
  )
  expect_equal(get_fixed_parameters(def), "Q")
})

test_that("get_fixed_parameters returns fixed params listed in model def even if null/empty", {
  j1 <- '{"fixed": null}'
  j2 <- '{"fixed": []}'
  res1 <- get_fixed_parameters(jsonlite::fromJSON(j1))
  res2 <- get_fixed_parameters(jsonlite::fromJSON(j2))
  expect_null(res1)
  expect_equal(res2, list())
})

test_that("get_fixed_parameters determines fixed params from omega matrix size", {
  def <- list(
    parameters = list(
      KA = 0.5,
      CL = 169L,
      V = 33.1,
      Q = 14.6,
      V2 = 226L,
      ALAG = 1L
    ),
    omega = c(0.223289, 0.449190, 3.459600)
  )

  expect_equal(
    get_fixed_parameters(def),
    c("V", "Q", "V2", "ALAG")
  )
})
