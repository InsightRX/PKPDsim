test_that("Simulating with table of parameters works", {
  skip_on_cran()
  parameters_table <- data.frame(
    CL = rnorm(10, 5, 5),
    V = rnorm(10, 5, 0.5)
  )

  par <- list(CL = 5, V = 50)
  reg <- new_regimen(amt = 2000, interval = 24, type = "infusion")
  covariates = list(WT = new_covariate(1))
  res <- sim_ode(
    ode = oral_1cmt_allometric,
    parameters_table = parameters_table,
    covariates = covariates,
    regimen = reg,
    only_obs = TRUE
  )

  expect_equal(length(unique(res$id)), 10)
  expect_equal(sum(is.na(res$y)), 0)
})

