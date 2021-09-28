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
t_obs <- seq(0, 36, 4)

test_that("mixture model works properly for single patient", {
  res0 <- sim_ode(mod, parameters = par, regimen = reg, covariates = covs, t_obs = t_obs, only_obs=T) # mixture_group not supplied
  res1 <- sim(mod, parameters = par, regimen = reg, t_obs = t_obs, covariates = covs, mixture_group = 1, only_obs=T)
  res2 <- sim(mod, parameters = par, regimen = reg, t_obs = t_obs, covariates = covs, mixture_group = 2, only_obs=T)
  expect_equal(round(res0[res0$t == 24,]$y, 2), 9.07) # should use whatever is in `parameters`
  expect_equal(round(res1[res1$t == 24,]$y, 2), 5.82)
  expect_equal(round(res2[res2$t == 24,]$y, 2), 1.15)
})

test_that("mixture model works properly when vectorized (using parameters_table)", {
  partab <- data.frame(CL = rep(0, 6), V = rep(50, 6))
  suppressMessages({
    expect_error(sim_ode(mod, parameters_table = partab, regimen = reg, t_obs = t_obs, covariates = covs, mixture_group = 1, only_obs=T))
    res1 <- sim(mod, parameters_table = partab, regimen = reg, t_obs = t_obs, covariates = covs, mixture_group = rep(1, 6), only_obs=T)
    res2 <- sim(mod, parameters_table = partab, regimen = reg, t_obs = t_obs, covariates = covs, mixture_group = rep(c(1,2), 3), only_obs=T, output_include = list(parameters = TRUE))
  })
  expect_equal(round(res1[res1$t == 24,]$y, 2), rep(5.82, 6))
  expect_equal(round(res2[res2$t == 24,]$y, 2), rep(c(5.82, 1.15), 3))
  expect_equal(res2[res2$id == 1,]$CL[1], 5)
  expect_equal(res2[res2$id == 2,]$CL[1], 15)
  expect_equal(res2[res2$id == 3,]$CL[1], 5)
})

test_that("mixture model works properly when vectorized (using covariates_table)", {
  covtab <- data.frame(ID = 1:8, WT = rep(seq(40, 130, 30), 2))
  suppressMessages({
    expect_error(sim(mod, parameters = par, covariates_table = covtab, regimen = reg, t_obs = t_obs, mixture_group = 1, only_obs=T))
    res <- sim(mod, parameters = par, covariates_table = covtab, regimen = reg, t_obs = t_obs, mixture_group = rep(c(1, 2), each=4), only_obs=T)
  })
  expect_equal(round(res[res$t == 24,]$y, 2), c(9.39, 5.82, 3.83, 2.65, 2.99, 1.15, 0.52, 0.25))
})

test_that("expected mixture model errors are caught", {
  mixture1 <- list(CL = list(values = c(5, 10), probability = 0.3))
  mixture2 <- list(
    CL = list(values = c(5, 10), probability = 0.3),
    V = list(values = c(10, 20), probability = 0.5)
  )
  mixture_multi <- list(
    CL = list(values = c(5, 6, 7), probability =c(0.3, 0.3, 0.4))
  )
  mixture_oops1 <- list(CL = list(values = c(5, 10), probability = 30))
  mixture_oops2 <- list(CL = list(values = c(5, 10), probability = -0.3))
  expect_error(check_mixture_model(mixture2, c("CL", "V")))
  expect_error(check_mixture_model(mixture1, c("Ke", "V")))
  expect_error(check_mixture_model(mixture_multi, c("CL", "V")))
  expect_error(check_mixture_model(mixture_oops1, c("CL", "V")))
  expect_error(check_mixture_model(mixture_oops2, c("CL", "V")))
})
