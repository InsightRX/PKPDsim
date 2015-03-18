install_github("ronkeizer/PKPDsim")
library(PKPDsim)
library(ggplot2)

p <- list(CL = 38.48,
          V  = 7.4,
          Q2 = 7.844,
          V2 = 5.19,
          Q3 = 9.324,
          V3 = 111)

r1 <- new_regimen(amt = 100,
                  interval = 24,
                  n = 10,
                  type = "infusion")

dat <- sim_ode (ode = pk_3cmt_iv,
                par = p,
                n_ind = 1,
                regimen = r1)

# block
omega <- c(0.3,       # IIV CL
           0.1, 0.3)  # IIV V

# as CV:
omega <- cv_to_omega (list(CL=0.3,
                           V=0.3), p)

# Plots
ggplot(dat, aes(x=t, y=y)) +
  geom_line() +
  scale_y_log10() +
  facet_wrap(~comp)

dat_iiv <- sim_ode (ode = pk_3cmt_iv,
                    par = p,
                    omega = omega,
                    n_ind = 10,
                    regimen = r1)

ggplot(dat_iiv, aes(x=t, y=y, colour=factor(id), group=id)) +
  geom_line() +
  scale_y_log10() +
  facet_wrap(~comp)

# now create a shiny app on-the-fly using the same parameters
# but with the sim_ode_shiny() function
sim_ode_shiny(ode = pk_3cmt_iv,
              par = p,
              regimen = new_regimen(amt=30),
              omega = omega)

p_efv <- list(CL = 10, V=300, KA=0.67)

sim_ode_shiny(ode = pk_1cmt_oral,
              parameters = p_efv,
              #              regimen = new_regimen (amt=600),
              omega = cv_to_omega (list(CL=0.2, V=0.1, KA=0.1), p_efv))
