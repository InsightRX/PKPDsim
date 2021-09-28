test_that("multiple regimens for multiple individuals can be simulated", {
  # Uses a model defined in `setup.R`
  cov_table <- data.frame(WT = rnorm(10, 70, 5))
  multi_regs <- list()
  for(i in seq(cov_table$WT)) {
    multi_regs[[i]] <- new_regimen(amt = 10 * cov_table$WT[i], interval = 12, type = "infusion")
  }
  class(multi_regs) <- "regimen_multiple"

  par <- list(CL = 5, V = 50)
  reg <- new_regimen(amt = 2000, interval = 24, type = "infusion")
  covariates = list(WT = new_covariate(1))
  res <- sim_ode(
    ode = mod_1cmt_iv,
    parameters = par,
    covariates = covariates,
    regimen = multi_regs,
    only_obs = TRUE
  )
  expect_equal(length(unique(res$id)),10)
  expect_equal(sum(is.na(res$y)), 0)
  # IDs ordered correctly:
  expect_true(length(unique(res$id)) == 10 && all(diff(res$id) >= 0))
})
