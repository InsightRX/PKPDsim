## Setup model + params
covs <- list(WT = PKPDsim::new_covariate(70))
mod <- new_ode_model(
  code = "
      dAdt[0] = -(CL*(WT/70.0)/V)*A[0];
    ",
  pk_code = " ",
  obs = list(cmt = 1, scale = "V"),
  mixture = list(CL = list(values = c(5, 15), probability = 0.3)),
  covariates = covs,
)
par <- list(CL = 3, V = 50)
reg <- new_regimen(amt = 250, n = 5, interval = 6, type = 'infusion', t_inf = 1)
t_obs <- seq(0, 360, 0.1)

test_that("mixture model works properly for single patient", {
  expect_warning({
    res0 <- sim_ode(mod, parameters = par, regimen = reg, covariates = covs, t_obs = t_obs, only_obs=T) # mixture_group not supplied
  })
  res1 <- sim_ode(mod, parameters = par, regimen = reg, t_obs = t_obs, covariates = covs, mixture_group = 1, only_obs=T)
  res2 <- sim_ode(mod, parameters = par, regimen = reg, t_obs = t_obs, covariates = covs, mixture_group = 2, only_obs=T)
  expect_equal(round(res0[res0$t == 24,]$y, 2), 1.15) # should peak most likely value
  expect_equal(round(res1[res1$t == 24,]$y, 2), 5.82)
  expect_equal(round(res2[res2$t == 24,]$y, 2), 1.15)
})

test_that("mixture model works properly when vectorized (using parameters_table)", {
  partab <- data.frame(CL = rep(0, 10), V = rep(50, 10))
  expect_error(sim_ode(mod, parameters_table = partab, regimen = reg, t_obs = t_obs, covariates = covs, mixture_group = 1, only_obs=T))
  res1 <- sim_ode(mod, parameters_table = partab, regimen = reg, t_obs = t_obs, covariates = covs, mixture_group = rep(1, 10), only_obs=T)
  res2 <- sim_ode(mod, parameters_table = partab, regimen = reg, t_obs = t_obs, covariates = covs, mixture_group = rep(c(1,2), 5), only_obs=T)
  expect_equal(round(res1[res1$t == 24,]$y, 2), rep(5.82, 10))
  expect_equal(round(res2[res2$t == 24,]$y, 2), rep(c(5.82, 1.15), 5))
})

test_that("mixture model works properly when vectorized (using covariates_table)", {
  covtab <- data.frame(ID = 1:20, WT = rep(seq(40, 130, 10), 2))
  expect_error(sim_ode(mod, parameters = par, covariates_table = covtab, regimen = reg, t_obs = t_obs, mixture_group = 1, only_obs=T))
  res <- sim_ode(mod, parameters = par, covariates_table = covtab, regimen = reg, t_obs = t_obs, mixture_group = rep(c(1, 2), each=10), only_obs=T)
  expect_equal(round(res[res$t == 24,]$y, 2), c(9.39, 7.94, 6.77, 5.82, 5.03, 4.38, 3.83, 3.37, 2.99, 2.65,
                                                2.99, 2.12, 1.55, 1.15, 0.87, 0.67, 0.52, 0.4, 0.31, 0.25))
})

