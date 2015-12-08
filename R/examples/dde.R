library(PKPDsim)
# library(deSolve)

p <- list(
  kel = 0.1,
  R0 = 15,
  TR = 48,
  Smax = 4,
  SC50 = 10,
  Vc = 1
)
reg <- new_regimen(amt = 100, interval = 12, n = 1)

dat <- sim_ode (dde = "lifespan_idr",
                p = p,
                regimen = reg, tmax = 60)

ggplot(dat %>% filter(comp=="obs"), aes(x=t, y=y)) + geom_line()

dat <- sim_ode_shiny (dde = "lifespan_idr",
                      p = p)
