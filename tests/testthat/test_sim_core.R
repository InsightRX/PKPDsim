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

test_that("sim core works for absorption model with lagtime", {  
  reg <- new_regimen(amt = 100, n = 5, interval = 12, t_inf = 1, type = "infusion")
  par <- list(CL = 5, V = 50, KA = 0.5, TLAG = 0.83)

  ## have to be explicit about t_obs with sim_core!
  f_ref <- function() {
    res <- sim(
      ode = mod_1cmt_oral_lagtime, 
      regimen = reg, 
      parameters = par, 
      only_obs = TRUE, 
      t_obs=c(0:24)
    )$y
    return(res)
  }
  f_core <- function() {
    obj <- sim(
      ode = mod_1cmt_oral_lagtime, 
      regimen = reg, 
      parameters = par, 
      only_obs = TRUE, 
      t_obs=c(0:24), 
      return_design=TRUE
    )
    sim_core(
      obj, 
      ode = mod_1cmt_oral_lagtime, 
      lagtime = c(0.83, 0)
    )$y ## Lagtime parameter needed!
  }
  expect_equal(f_ref(), f_core())
})
