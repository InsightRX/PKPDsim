library(devtools)
install_github("ronkeizer/PKPDsim")
library(PKPDsim)
library(ggplot2)
library(plyr)
library(survival)

## parameters
p <- list(CL = 38.48,
          V  = 7.4,
          KA = 0.5,
          LAMBDA = 0.1)
omega <- cv_to_omega (par_cv = list(CL=0.2, V=0.2), p)

## set up simulations
dose <- c(10, 20, 40, 80)
dat <- c()

## simulate
for (i in seq(dose)) {
  dat <- rbind(dat, cbind(dose = dose[i], sim_ode (ode = "pk_tte_1cmt_oral_exp_hazard",
                                          par = p,
                                          omega = omega,
                                          n_ind = 20,
                                          regimen = new_regimen(amt = dose[i], interval = 12, n = 6),
                                          t_tte = (1:5)*24,
                                          t_obs = 1:100,
                                          rtte = FALSE) ) )
}

# Plots
ggplot(dat[dat$comp == 3,], aes(x=t, y=y, group=id)) +
  geom_line() +
  scale_y_log10() +
  facet_grid(dose ~ comp, scales="free_y") + theme_empty()

## Kaplan-Meier
obs <- data.frame(dat[dat$comp == "event",])
fit <- survfit(Surv(obs$t, obs$y ) ~ obs$dose, conf.type ="none")
plot(fit, col = 1:4)
