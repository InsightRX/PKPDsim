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
    "For compartments where bioavailability is specified"
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
    type = c("d1", "d2", "d1", "d2", "d2") # must correspond to dose_cmt arg
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
  expect_message(
    {
      c <- regimen_to_nm(
        a,
        t_obs = c(1, 2, 3),
        dose_cmt = c(1, 2, 1, 2, 2),
        bioav = c(0.5, 1)
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
  expect_true(all(expected_cols %in% colnames(c)))
  # in NONMEM, oral doses have a RATE of zero, which indicates a bolus dose
  expect_equal(b$RATE, c(0, 0, 0, 0, 0, 10, 10, 0))
  expect_equal(c$RATE, c(0, 0, 0, 0, 0, 10, 20, 0))
})

test_that("throws warning when bioav specified as model parameter and need to convert RATE, but not when not needed", {
  a <- new_regimen(
    amt = 10,
    time = c(1, 2, 3, 4),
    t_inf = c(0, 0, 1, 1),
    type = c("oral", "oral", "infusion", "infusion")
  )
  expect_message({
    b <- regimen_to_nm(
      a,
      dose_cmt = c(1, 1, 2, 2),
      t_obs = c(1, 2, 3),
      bioav = c("Fi", 1)
    )
  }, "Recalculating infusion rates")
  a2 <- new_regimen(
    amt = 10,
    time = c(1, 2, 3, 4),
    t_inf = c(1, 1, 1, 1),  # now an infusion in the sc compartment and infusion lengths are >0, should throw warning!
    type = c("sc", "sc", "infusion", "infusion")
  )
  expect_warning(
    regimen_to_nm(
      a2,
      dose_cmt = c(1, 1, 2, 2),
      t_obs = c(1, 2, 3),
      bioav = c("Fi", 1)
    ),
    "For compartments where bioavailability is specified"
  )
})
