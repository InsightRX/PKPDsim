context("Compartment mapping")

pk1cmt_oral_code <- new_ode_model(
  code = "dAdt[1] = -KA*A[1]; dAdt[2] = KA*A[1] - (CL/V)*A[2];",
  obs = list(cmt = 2, scale="V"),
  cmt_mapping = list(oral = 1, infusion = 2, bolus = 2)
)

test_that("Compartment mapping is added to attributes", {
  expect_equal(attr(pk1cmt_oral_code, "cmt_mapping")[["oral"]], 1)
  expect_equal(attr(pk1cmt_oral_code, "cmt_mapping")[["infusion"]], 2)
})

test_that("Admin route is interpreted and simulated correctly", {
  regimen <- new_regimen(
    amt = c(100, 100, 100, 100),
    times = c(0, 12, 24, 36),
    type = c("oral", "oral", "infusion", "infusion"),
    t_inf = c(0, 0, 1, 1)
  )
  p <- list(KA = 1, CL = 5, V = 50)
  res <-  sim_ode(
    ode = pk1cmt_oral_code,
    parameters = p,
    regimen = regimen,
    only_obs = FALSE
  )
  expect_equal(round(res$y[res$comp == 1 & res$t == 25], 4), 2e-04)
  expect_true(res$y[res$comp == 2 & res$t == 25] >= 100)
})

