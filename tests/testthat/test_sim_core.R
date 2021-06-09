test_that("sim core works", {
  # Uses model defined in setup.R

  reg <- new_regimen(amt = 100, n = 5, interval = 12, t_inf = 1, type = "infusion")
  par <- list(KA = 1, CL = 5, V = 50)

  ## have to be explicit about t_obs with sim_core!
  f1 <- function() {
    res <- sim(ode = mod_1cmt_oral, regimen = reg, parameters = par, only_obs = TRUE, t_obs=c(0:24))$y
    return(res)
  }

  f2 <- function() {
    obj <- sim(ode = mod_1cmt_oral, regimen = reg, parameters = par, only_obs = TRUE, t_obs=c(0:24), return_design=TRUE)
    sim_core(obj, ode = mod_1cmt_oral)$y
  }
  expect_equal(f1(), f2())
})
