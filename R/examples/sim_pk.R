library(PKPDsim)
library(ggplot2)

p <- list(CL = 38.48,
          V  = 7.4,
          Q2 = 7.844,
          V2 = 5.19,
          Q3 = 9.324,
          V3 = 111)
r1 <- new_regimen(amt = 100,
              times = c(0, 24, 36),
              type = "infusion")
dat <- sim_ode (ode = pk_3cmt_iv,
                par = p,
                regimen = r1)
omega <- c(0.3,       # IIV CL
           0.1, 0.3)  # IIV V

# Plots
ggplot(dat, aes(x=t, y=y)) +
  geom_line() +
  scale_y_log10() +
  facet_wrap(~comp)

dat_iiv <- sim_ode (ode = pk_3cmt_iv,
                    par = p,
                    omega = omega,
                    n_ind = 10,
                    regimen = regimen)
ggplot(dat_iiv, aes(x=t, y=y, colour=factor(id), group=id)) +
  geom_line() +
  scale_y_log10() +
  facet_wrap(~comp)

# now create a shiny app on-the-fly using the same parameters
# but with the sim_ode_shiny() function
sim_ode_shiny(ode = pk_3cmt_iv,
              par = p,
              omega = omega)


