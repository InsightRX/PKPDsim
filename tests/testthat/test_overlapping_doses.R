## Uses model defined in setup.R

test_that("continuous infusion with overlapping doses are simulated ok", {
  reg <- new_regimen(
    amt = c(100, 100, 100, 100),
    times = c(0, 6, 7, 12, 13),
    type = "infusion",
    t_inf = c(2, 2, 2, 2, 2)
  )
  par <- list(CL = 5, V = 50)
  ## RK: needs to be run twice to confirm no memory issues!
  res <- sim_ode(mod_1cmt_iv, parameters = par, reg = reg, only_obs = TRUE)
  res <- sim_ode(mod_1cmt_iv, parameters = par, reg = reg, only_obs = TRUE)

  expect_true(res[res$t == 7,]$y > res[res$t == 6,]$y)
  expect_true(res[res$t == 8,]$y > res[res$t == 7,]$y)
  expect_true(res[res$t == 9,]$y > res[res$t == 8,]$y)
  expect_true(diff(res[res$t %in% c(7,8),]$y) > diff(res[res$t %in% c(8,9),]$y))

  reg2 <- new_regimen(
    amt = c(100, 100, 100, 100),
    times = c(0, 6, 12, 18),
    type = "infusion",
    t_inf = c(2, 6, 2, 6)
  )
  res2 <- sim_ode(mod_1cmt_iv, parameters = par, reg = reg2, only_obs = TRUE)
  expect_true(res2[res2$t == 12,]$y[1] > res2[res2$t == 6,]$y[1])
  expect_true(res2[res2$t == 14,]$y[1] > res2[res2$t == 12,]$y[1])
  expect_true(res2[res2$t == 18,]$y[1] < res2[res2$t == 14,]$y[1])
})

