test_that("Required column checks occur", {
  expect_error(
    nm_to_regimen(
      data.frame(EVID = 0, AMT = 100)
    ),
    "TIME column is required"
  )

  expect_error(
    nm_to_regimen(
      data.frame(EVID = 0, TIME = 0)
    ),
    "AMT column is required"
  )

  expect_error(
    nm_to_regimen(
      data.frame(AMT = 100, TIME = 0)
    ),
    "EVID column is required"
  )
})

test_that("NM-style dataframe for 1 ID converted to regimen", {
  pt1 <- data.frame(
    ID = 1,
    EVID = c(1, 1, 0, 1, 1, 0),
    AMT = c(100, 100, 0, 200, 200, 0),
    TIME = c(0, 12, 23, 25, 36, 47),
    RATE = c(100, 100, 0, 200, 400, 0),
    DV = c(0, 0, 5, 0, 0, 12)
  )
  reg1 <- nm_to_regimen(pt1)
  expect_true(inherits(reg1, "regimen"))
  expect_equal(reg1$dose_amts, c(100, 100, 200, 200))
  expect_equal(reg1$dose_times, c(0, 12, 25, 36))
  expect_equal(reg1$t_inf, c(1, 1, 1, 0.5))
  expect_equal(reg1$type, rep("infusion", 4))
})

test_that("Bolus/oral doses handled", {
  pt2 <- data.frame(
    ID = 1,
    EVID = c(1, 1, 0, 1, 1, 0),
    AMT = c(100, 100, 0, 200, 200, 0),
    TIME = c(0, 12, 23, 25, 36, 47),
    DV = c(0, 0, 5, 0, 0, 12)
  )
  reg2 <- nm_to_regimen(pt2)
  expect_true(inherits(reg2, "regimen"))
  expect_equal(reg2$dose_amts, c(100, 100, 200, 200))
  expect_equal(reg2$dose_times, c(0, 12, 25, 36))
  expect_equal(reg2$t_inf, rep(0, 4))
  expect_equal(reg2$type, rep("bolus", 4))
})

test_that("Multiple regimens from NONMEM-style dataset", {
  nm <- data.frame(
    ID = c(1, 1, 1, 2, 2, 2),
    EVID = c(1, 1, 0, 1, 1, 0),
    AMT = c(100, 100, 0, 200, 200, 0),
    TIME = c(0, 12, 23, 0, 24, 47),
    DV = c(0, 0, 5, 0, 0, 12)
  )
  multi_regs <- nm_to_regimen(nm)
  expect_true(inherits(multi_regs, "regimen_multiple"))
  expect_true(inherits(multi_regs[[1]], "regimen"))

  expect_equal(multi_regs[[1]]$dose_times, c(0, 12))
  expect_equal(multi_regs[[2]]$dose_times, c(0, 24))
  expect_equal(multi_regs[[1]]$dose_amts, c(100, 100))
  expect_equal(multi_regs[[2]]$dose_amts, c(200, 200))
})
