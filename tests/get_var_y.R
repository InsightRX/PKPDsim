Sys.setenv("R_TESTS" = "")
# test delta method
library(PKPDsim)
library(testit)

## Test derivative function
assert("Correct calculation of derivatives", round(PKPDsim:::calc_dydP(1:5 + rep(.2, 5), 1:5, .1, FALSE), 3) == rep(2, 5))
assert("Correct calculation of derivatives, log scale", round(PKPDsim:::calc_dydP(1:5 + rep(.2, 5), 1:5, .1, TRUE), 3) == c(1.823, 0.953, 0.645, 0.488, 0.392))

## Regimen for testing:
reg <- PKPDsim::new_regimen(amt = 100, n = 3, interval = 12, type = "infusion", t_inf = 2)

## 1 compartment model
mod <- new_ode_model("pk_1cmt_iv")
par <- list(CL = 5, V = 50)
omega <- c(0.1,
           0.0, 0.1)
t_obs <- c(2, 48)
res <- PKPDsim::sim_ode(mod, parameters = par, regimen = reg, t_obs = t_obs, only_obs=TRUE)

## Test basic functionality
v1 <- PKPDsim::get_var_y( ## delta rule
  model = mod, parameters = par, t_obs = t_obs,
  regimen = reg, omega = omega, method="delta")
v2 <- PKPDsim::get_var_y( ## shitload of simulations (n=20,000)
  model = mod, parameters = par, t_obs = t_obs, auc = FALSE,
  regimen = reg, omega = omega, method="sim", n_ind = 2500)
# v3 <- PKPDsim::get_var_y( ## halton sampling (n=250) # much faster way of getting SD
#   model = mod, parameters = par, t_obs = t_obs, regimen = reg, omega = omega,
#   method="sim", sequence="halton",
#   n_ind = 1000)
assert("PK 1cmt model: difference in SD < 10%",
  all(abs(((v1$regular-v2$regular)/res$y)) < 0.1)
)
# assert("PK 1cmt model: difference brute force vs Halton seq < 20%",
#   all(abs(((v2$regular-v3$regular)/res$y)) < 0.2)
# )

## get CI instead of SD
t_obs2 <- c(2,12)
v1a <-
  PKPDsim::get_var_y( ## delta rule, parametric
  model = mod, parameters = par, t_obs = t_obs2, regimen = reg, omega = omega, q=c(0.05,.95))
v2a <- PKPDsim::get_var_y( ## shitload of simulations (n=10,000), non-parametric
  model = mod, parameters = par, t_obs = t_obs2, regimen = reg, omega = omega,
  method="sim", n_ind=2500, q=c(0.05, .95))
# v3a <- PKPDsim::get_var_y( ## halton sampling (n=250) # much faster way of getting SD
#   model = mod, parameters = par, t_obs = t_obs2, regimen = reg, omega = omega,
#   method="sim", sequence="halton", q=c(0.05, .95), n_ind = 500)
# assert("difference in CI brute force vs Halton seq < 10%",
#        all(abs(((v2a$regular-v3a$regular)/v2a$regular)) < 0.1)
# )

# comp <- data.frame(cbind(t = res$t, dv=res$y, jac = v1, sim = v2))
# comp.m <- melt(comp, id=c("t"))
# ggplot(comp.m[comp.m$variable != "dv",], aes(x=t, y=value, colour=variable)) + geom_line()
#
# comp2 <- data.frame(cbind(t = res$t, dv=res$y, jac = v1, sim = v2))
# ggplot(comp2, aes(x=t, y=dv)) +
#   geom_ribbon(aes(ymin = dv - sim, ymax=dv + sim), fill=rgb(0,0.5,0.5,0.5)) +
#   geom_ribbon(aes(ymin = dv - jac, ymax=dv + jac), fill=rgb(0.5,0,0.5,0.5)) +
#   geom_line()

## 2 compartment model
mod2 <- new_ode_model("pk_2cmt_iv")
par2 <- list(CL = 1, V = 10, Q = 1, V2 = 10)
omega2 <- c(0.118,
            0.05, 0.143,
            0.01, 0.01, .29,
            0.01, 0.01, 0.07, .102)
res <- PKPDsim::sim_ode(mod2, parameters = par2, t_obs = t_obs, regimen = reg, only_obs=TRUE)
v1 <- PKPDsim::get_var_y(model = mod2, parameters = par2,t_obs = t_obs, regimen = reg, omega = omega2)
v2 <- PKPDsim::get_var_y(model = mod2, parameters = par2, t_obs = t_obs, regimen = reg, omega = omega2, method="sim", n_ind=2000)
# v3 <- PKPDsim::get_var_y(model = mod2, parameters = par2, t_obs = t_obs, regimen = reg, omega = omega2, method="sim", sequence="halton", n_ind=250)
assert("PK 2cmt model: difference in SD < 10%",
 all(abs(((v1$regular-v2$regular)/res$y)) < 0.1)
)
# assert("PK 2cmt model: difference in SD < 10%",
#   all(abs(((v2$regular-v3$regular)/res$y)) < 0.1)
# )

# comp <- data.frame(cbind(t = res$t, dv=res$y, jac = v1, sim = v2))
# comp.m <- melt(comp, id=c("t"))
# ggplot(comp.m[comp.m$variable != "dv",], aes(x=t, y=value, colour=variable)) + geom_line()
#
# comp2 <- data.frame(cbind(t = res$t, dv=res$y, jac = v1, sim = v2))
# ggplot(comp2, aes(x=t, y=dv)) +
#   geom_ribbon(aes(ymin = dv - sim, ymax=dv + sim), fill=rgb(0 ,0.5,0.5,0.5)) +
#   geom_ribbon(aes(ymin = dv - jac, ymax=dv + jac), fill=rgb(0.5,0,0.5,0.5)) +
#   geom_line()

## 1 compartment model with MM elimination
mod3 <- new_ode_model("pk_1cmt_iv_mm")
par3 <- list(VMAX = 5, KM = 5, V = 10)
omega3 <- c(0.1,
            0.05, 0.1,
            0.01, 0.01, 0.1)
res <- PKPDsim::sim_ode(mod3, parameters = par3, regimen = reg, t_obs = t_obs, only_obs=TRUE)
v1 <- PKPDsim::get_var_y(model = mod3, parameters = par3, t_obs = t_obs, regimen = reg, omega = omega3)
v2 <- PKPDsim::get_var_y(model = mod3, parameters = par3, t_obs = t_obs, regimen = reg, omega = omega3, method="sim", n_ind=4000)
# v3 <- PKPDsim::get_var_y(model = mod3, parameters = par3, t_obs = t_obs, regimen = reg, omega = omega3, method="sim", sequence="halton", n_ind=500)
assert("PK 1cmt MM model: relative difference in SD < 10%",
  all(abs(((v1$regular-v2$regular)/res$y)) < 0.1)
)
# assert("PK 1cmt MM model: relative difference in SD < 5%",
#   all(abs(((v2$regular-v3$regular)/res$y)) < 0.05)
# )
# comp <- data.frame(cbind(t = res$t, dv=res$y, jac = v1, sim = v2))
# comp.m <- melt(comp, id=c("t"))
# ggplot(comp.m[comp.m$variable != "dv",], aes(x=t, y=value, colour=variable)) + geom_line()
#
# comp3 <- data.frame(cbind(t = res$t, dv=res$y, jac = v1, sim = v2))
# ggplot(comp3, aes(x=t, y=dv)) +
#   geom_ribbon(aes(ymin = dv - sim, ymax=dv + sim), fill=rgb(0, 0.5,0.5,0.4)) +
#   geom_ribbon(aes(ymin = dv - jac, ymax=dv + jac), fill=rgb(0.5, 0,0.5,0.4)) +
#   geom_line()
#
