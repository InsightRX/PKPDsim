test_that("now_utc returns time in UTC", {
  mockery::stub(
    now_utc,
    "Sys.time",
    structure(1640042187.11864, class = c("POSIXct", "POSIXt"))
  )
  now <- now_utc()
  expect_equal(attr(now, "tzone"), "UTC")
})

test_that("print_list returns expected results", {
  l1 <- list(x = 1, y = 2)
  l2 <- list(x = 1, y = "foo")
  l3 <- list(x = list(y = "foo"), z = "bar")
  res1 <- print_list(l1)
  res2 <- print_list(l2)
  res3 <- print_list(l3)
  expect_equal(res1, "list(x = 1, y = 2)")
  expect_equal(res2, "list(x = 1, y = \"foo\")")
  expect_equal(res3, "list(x = list(y = \"foo\"), z = \"bar\")")
})

test_that("print_list can remove outer list() call", {
  l1 <- list(x = 1, y = 2)
  l2 <- list(formulation = c("a", "b"))
  l3 <- list(x = list(y = list(z = 1)))
  expect_equal(print_list(l1, wrapper = FALSE), "x = 1, y = 2")
  expect_equal(print_list(l2, wrapper = FALSE), "formulation = c(\"a\", \"b\")")
  expect_equal(print_list(l3, wrapper = FALSE), "x = list(y = list(z = 1))")
})

test_that("print_list supports empty lists", {
  x <- list()
  res <- print_list(x)
  expect_equal(res, "")
})

test_that("print_list supports NULL input", {
  expect_equal(print_list(NULL), "")
})
