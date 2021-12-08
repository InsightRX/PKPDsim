test_that("Simulating with table of parameters works", {
  skip_on_cran()
  parameters_table <- data.frame(
    CL = rnorm(10, 5, 5),
    V = rnorm(10, 5, 0.5)
  )

  mod <- new_ode_model(
    code = "
      dAdt[1] = -(CL/V) * A[1];
    ",
    obs = list(cmt = 1, scale = "V"),
    covariates = list(WT = new_covariate(70)),
    cpp_show_code=FALSE
  )
  par <- list(CL = 5, V = 50)
  reg <- new_regimen(amt = 2000, interval = 24, type = "infusion")
  covariates = list(WT = new_covariate(1))
  res <- sim_ode(
    ode = mod,
    parameters_table = parameters_table,
    covariates = covariates,
    regimen = reg,
    only_obs = TRUE
  )

  expect_equal(length(unique(res$id)), 10)
  expect_equal(sum(is.na(res$y)), 0)
})

