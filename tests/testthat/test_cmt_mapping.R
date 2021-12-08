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

test_that("multiple scaling types on one compartment works", {
  skip_on_cran()
  mod <- new_ode_model(
    code = "
      dAdt[1] = -KA * A[1];
      dAdt[2] = -(CL/V) * A[2] + KA*A[1];
    ",
    obs = list(
      cmt = c(2, 2),
      scale = c(1, "V"),
      label = c("abs", "conc")
    ),
    cpp_show_code = FALSE
  )
  par <- list(CL = 5, V = 50, KA = .5)
  reg <- new_regimen(amt = 100, n = 5, interval = 12)
  res <- sim_ode(
    ode = mod,
    parameters = par,
    regimen = reg,
    only_obs = TRUE
  )
  dat <- cbind(res[res$comp == "abs",]$y, res[res$comp == "conc",]$y)
  expect_true("PKPDsim" %in% class(mod))
  expect_equal(length(unique(res$comp)), 2)
  expect_equal(round(dat[,1],1), round(dat[,2]*par$V,1))
})
