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

test_that("regimen with infusion correctly recalculates rates when bioavailability specified", {
  a <- new_regimen(amt = 10, n = 5, interval = 12, t_inf = 1, type = "infusion")
  expect_message(
    {
      b <- regimen_to_nm(
        a,
        t_obs = c(1, 2, 3),
        bioav = 0.5
      )
    },
    "Recalculating infusion rates"
  )
  expect_warning(
    regimen_to_nm(
      a,
      t_obs = c(1, 2, 3),
      bioav = "F1"
    ),
    "Bioavailability not specified correctly"
  )
  expected_cols <- c(
    "ID",
    "TIME",
    "CMT",
    "DV",
    "AMT",
    "EVID",
    "MDV",
    "RATE"
  )
  expect_true(all(expected_cols %in% colnames(b)))
  expect_equal(b$RATE, c(5, 0, 0, 0, 5, 5, 5, 5))
})

test_that("rate is calculated for any regimen with an infusion length", {
  a <- new_regimen(
    amt = 10,
    n = 5,
    interval = 12,
    t_inf = c(0, 0, 0.5, 0.5, 0),
    type = "drug_1"
  )
  expect_message(
    {
      b <- regimen_to_nm(
        a,
        t_obs = c(1, 2, 3),
        bioav = 0.5
      )
    },
    "Recalculating infusion rates"
  )
  expected_cols <- c(
    "ID",
    "TIME",
    "CMT",
    "DV",
    "AMT",
    "EVID",
    "MDV",
    "RATE"
  )
  expect_true(all(expected_cols %in% colnames(b)))
  # in NONMEM, oral doses have a RATE of zero, which indicates a bolus dose
  expect_equal(b$RATE, c(0, 0, 0, 0, 0, 10, 10, 0))
})

