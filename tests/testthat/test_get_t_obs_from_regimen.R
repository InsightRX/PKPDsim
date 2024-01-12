test_that("Appropriate t obs returned", {
  reg <- new_regimen(amt = 100, type = "infusion", n = 3, interval = 12)
  expect_equal(get_t_obs_from_regimen(reg, extra_t_obs = FALSE), 0:36)
  reg <- new_regimen(amt = 100, type = "oral", n = 3, interval = 12)
  expect_equal(get_t_obs_from_regimen(reg, extra_t_obs = FALSE), 0:36)
  reg <- new_regimen(amt = 100, type = "infusion", n = 3, interval = 12, t_inf = 0.33)
  expect_equal(get_t_obs_from_regimen(reg, extra_t_obs = FALSE), c(0:36, 0.33))
  reg <- new_regimen(amt = 100, type = "inf_drug", n = 3, interval = 12, t_inf = 0.33)
  expect_equal(get_t_obs_from_regimen(reg, extra_t_obs = FALSE), c(0:36, 0.33))
})

