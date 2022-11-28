test_that("now_utc returns time in UTC", {
  mockery::stub(
    now_utc,
    "Sys.time",
    structure(1640042187.11864, class = c("POSIXct", "POSIXt"))
  )
  now <- now_utc()
  expect_equal(attr(now, "tzone"), "UTC")
})

test_that("print_list supports empty lists", {
  x <- list()
  res <- print_list(x)
  expect_equal(res, "")
})
