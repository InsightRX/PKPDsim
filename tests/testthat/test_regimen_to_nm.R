test_that("regimen can be converted to nonmem format", {
  a <- new_regimen(amt = 10, n = 5, interval = 12)
  b <- regimen_to_nm(a, t_obs = c(1, 2, 3))
  expect_equal(nrow(b), 8)
  expect_equal(ncol(b), 7)
  expected_cols <- c(
    "ID",
    "TIME",
    "CMT",
    "DV",
    "AMT",
    "EVID",
    "MDV"
  )
  expect_true(all(expected_cols %in% colnames(b)))
})

