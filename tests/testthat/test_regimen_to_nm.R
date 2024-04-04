test_that("regimen can be converted to nonmem format", {
  a <- new_regimen(amt = 10, n = 5, interval = 12)
  expect_warning({
    b <- regimen_to_nm(a, t_obs = c(1, 2, 3))
  })
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
  expect_warning({
    expect_message({
        b <- regimen_to_nm(
          a,
          t_obs = c(1, 2, 3),
          bioav = 0.5
        )
      },
      "Applying bioavailability to AMT and RATE column."
    )
  })
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

test_that("bioavailability specified as vector for compartments", {
  reg_comb <- new_regimen(
    amt = 1,
    n = 5,
    interval = 12,
    type = "oral",
    cmt = c(1, 2, 1, 2, 1)
  )
  expect_message({
    res1 <- regimen_to_nm(
      reg_comb,
      t_obs = c(1, 2, 3),
      bioav = c(1, 2)
    )
  })
  expect_message(
    expect_warning(
      res2 <- regimen_to_nm(
        reg_comb,
        t_obs = c(1, 2, 3),
        dose_cmt = c(1, 2, 3, 2, 1),
        bioav = c(1, 2) # bioav not defined for all compartments, so throw warning
      )
    )
  )
  expect_equal(res1$AMT, c(1, 0, 0, 0, 2, 1, 2, 1))
  expect_equal(res2$AMT, c(1, 0, 0, 0, 2, 1, 2, 1))
})

test_that("rate is calculated for any regimen with an infusion length", {
  a <- new_regimen(
    amt = 10,
    n = 5,
    interval = 12,
    t_inf = c(0, 0, 0.5, 0.5, 0),
    type = c("d1", "d2", "d1", "d2", "d2") # must correspond to dose_cmt arg
  )
  expect_warning({
    expect_message({
      b <- regimen_to_nm(
        a,
        t_obs = c(1, 2, 3),
        bioav = 0.5
      )
    },
    "Applying bioavailability to AMT and RATE column. "
    )
  })
  expect_message(
    {
      c <- regimen_to_nm(
        a,
        t_obs = c(1, 2, 3),
        dose_cmt = c(1, 2, 1, 2, 2),
        bioav = c(0.5, 1)
      )
    },
    "Applying bioavailability"
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
  # in NONMEM, oral doses have a RATE of zero (if RATE in dataset), which indicates a bolus dose
  expect_equal(b$RATE, c(0, 0, 0, 0, 0, 10, 10, 0))
  expect_equal(c$RATE, c(0, 0, 0, 0, 0, 10, 20, 0))
})
