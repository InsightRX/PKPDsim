## Test t_init functionality
##
library(testit)
library(PKPDsim)
Sys.setenv("R_TESTS" = "")

## e.g. TDM before first dose:
## at t=-8, conc=10000
## Use this as true value in the simulations
par   <- list(CL = 7.67, V = 97.7)
mod <- new_ode_model(code = "
                     dAdt[1] = -(CL/V)*A[1];
                     ", parameters = par, obs = list(cmt = 1, scale = "V"), cpp_show_code=F)
omega <- c(0.0406,
           0.0623, 0.117)
ruv_single <- list(prop = 0.1, add = 1)
ruv_multi <- list(prop = c(0.1, 1), add = c(1, 20))
reg <- new_regimen(amt = 100000, times=c(0, 24), type="bolus")

s_single <- PKPDsim::sim(ode = mod,
                  parameters = par,
                  n_ind = 1,
                  regimen = reg,
                  only_obs = TRUE,
                  t_obs = c(2,4,6,8),
                  seed = 123456,
                  res_var = ruv_single)

## specified as multi, but obs_type is all 1, so should give same results as s_single
s_multi1 <- PKPDsim::sim(ode = mod,
                         parameters = par,
                         n_ind = 1,
                         regimen = reg,
                         only_obs = TRUE,
                         t_obs = c(2,4,6,8),
                         obs_type = c(1, 1, 1, 1),
                         seed = 123456,
                         res_var = ruv_multi,
                         t_init = 10)

assert("check multi-obs does not throw error", sum(s_single$y - s_multi1$y) < 1e-5)

## should now give different results
s_multi2 <- PKPDsim::sim(ode = mod,
                         parameters = par,
                         n_ind = 1,
                         regimen = reg,
                         only_obs = TRUE,
                         t_obs = c(2,4,6,8),
                         obs_type = c(1, 2, 1, 2),
                         seed = 123456,
                         res_var = ruv_multi,
                         t_init = 10)
assert("check multi-obs implements same for obs_type1", sum(abs(s_single$y[c(1,3)] - s_multi2$y[c(1,3)])) < 1e-5)
assert("check multi-obs implements different error for obs_type2", sum(abs(s_single$y[c(2,4)] - s_multi2$y[c(2,4)])) > 50)
