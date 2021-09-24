test_that("na_locf() fills in NAs with previous non-missing value", {
  a <- c(1, 2, NA, 3)
  b <- c(NA, NA, 5, 7, NA, 9)
  c <- c(1, 2, 3)
  expect_equal(na_locf(a), c(1, 2, 2, 3))
  expect_equal(na_locf(b), c(NA, NA, 5, 7, 7, 9))
  expect_equal(na_locf(c), c(1, 2, 3))
})

test_that("fromLast arg to na_locf() works", {
  a <- c(NA, 3, 2, 1)
  b <- c(5, NA, 7, 2, NA)
  expect_equal(na_locf(a, fromLast = TRUE), c(3, 3, 2, 1))
  expect_equal(na_locf(b, fromLast = TRUE), c(5, 7, 7, 2, NA))
})
