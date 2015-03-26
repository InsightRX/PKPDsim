library (deSolve)
library (dplyr)
library (ggplot2)
library (PKPDsim)

# Doses:
# Give 2 doses of 100 mg, but then simulate out 19 intervals more
# without actually giving a dose
regimen <- list(amt = 100,
                interval = 24,
                n = 2,
                type = "infusion")

# docetaxel PK:
p <- list(CL = 38.48,
          V  = 7.4,
          Q2 = 7.844,
          V2 = 5.19,
          Q3 = 9.324,
          V3 = 111,
          mtt = 89.3,
          circ0 = 5.05, # actually 5.05e9, modeling on scale of 1e9
          gam = 0.163,
          EC50 = 7.17,
          Emax = 83.9)

pk_1cmt_iv

combine_models <- c()

# Simulate:
dat <- sim_ode (ode = "pkpd_hemtox",
                p = p,
                regimen = new_regimen(),
                A_init = c(rep(0, 3), rep(p$circ0, 5)),
                step_size = 0.25,
                tmax = 24*21)

### Plot all compartments:
ggplot(dat %>% filter(comp=="obs"), aes(x=t, y=y)) +
  geom_line() +
  scale_y_log10() +
  facet_wrap(~comp)

### Only neutrophils:
ggplot(dat %>% filter(comp == 8), aes(x=t/24, y=ipred)) +
  geom_line() +
  geom_hline(yintercept = p$circ0, linetype='dotted') +
  geom_hline(yintercept = 1, linetype='dashed', colour="red") +  # <1e9 is considered neutropenia according CTC
  scale_y_log10(breaks=c(0.1, 1, 5, 10), limits=c(.01, 10)) +
  ylab("Neutrophils (1e9)") + xlab("Time in days")

## Create shiny app
sim_ode_shiny (ode = hemtox,
               p = p,
               regimen = regimen,
               A_init = c(rep(0, 3), rep(p$circ0, 5)),
               tmax = 21*24)
