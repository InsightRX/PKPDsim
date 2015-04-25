# benchmark script from Devin Pastoor

library(ggplot2)
library(PKPDsim)
library(microbenchmark)

p <- list(CL = 1,
          V  = 10)

r1 <- new_regimen(amt = 100,
                  times = c(0, 12, 24, 36, 48),
                  type = "bolus")

## run once to compile function
sim_ode (ode = "pk_1cmt_iv",
         par = p,
         regimen = r1,
         step_size = 0.016,
         t_max = 72,
         cpp = TRUE)

microbenchmark(sim_ode (ode = "pk_1cmt_iv",
                        par = p,
                        regimen = r1,
                        step_size = 0.016,
                        t_max = 72,
                        cpp = TRUE, cpp_recompile=FALSE),
               times = 10L)

sim_ode_1000 <- function() {
  sim_ode (ode = "pk_1cmt_iv",
           par = p,
           n_ind = 1000,
           regimen = r1,
           step_size = 0.25,
           t_max = 72,
           cpp = TRUE, cpp_recompile = FALSE)
}

microbenchmark(sim_ode_1000(), times = 2L)
