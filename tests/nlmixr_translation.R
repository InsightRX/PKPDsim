library(PKPDsim)
library(testit)
Sys.setenv("R_TESTS" = "")

mod   <- new_ode_model("pk_1cmt_iv")
par   <- list(CL = 5, V = 50)
ruv   <- list(prop = 0.1, add = 1)
omega <- c(0.1,
           0.05, 0.1)

## Create nlmixr model object
f <- pkpdsim_to_nlmixr(
  model = mod,
  parameters = par,
  omega = c(0.1, 0.05, 0.1),
  res_var = list(prop = 0.1, add = 0.1),
  log_transform = T)

testit::assert("Simple test if function is returned", class(f) == "function")
