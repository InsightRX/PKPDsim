test_that("now_utc returns time in UTC", {
  mockery::stub(
    now_utc,
    "Sys.time",
    structure(1640042187.11864, class = c("POSIXct", "POSIXt"))
  )
  now <- now_utc()
  expect_equal(attr(now, "tzone"), "UTC")
})
