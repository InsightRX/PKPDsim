# Test: numeric accuracy
# compare with analytic equations

library(PKPDsim)
library(testit) ## testthat doesn't play nice with sourceCpp

Sys.setenv("R_TESTS" = "")

## parameters:
p <- list(KA = 1, CL = 5, V = 50)
t_obs <- c(0:72)
t_obs2 <- t_obs + 0.1234 # also needs to be producing results with non-integer times
dose <- 100
t_dose <- c(0)
regimen <- new_regimen(amt=dose, times = t_dose, type="oral")

## models:
pk1cmt_oral_anal = function(t, dose, KA, V, CL) {
  dose*KA/(V*(KA-CL/V))*(exp(-(CL/V) * t)-exp(-KA * t))
}
pk1cmt_oral_lib  <- new_ode_model("pk_1cmt_oral")
pk1cmt_oral_code <- new_ode_model(code = "
                                  dAdt[1] = -KA*A[1];
                                  dAdt[2] = KA*A[1] - (CL/V)*A[2];", obs=list(cmt = 2, scale="V")
)

res <- list()
res$pk1cmt_oral_lib <- sim_ode(
  ode=pk1cmt_oral_lib,
  par=p,
  regimen=regimen,
  t_obs=t_obs,
  int_step_size = 1,
  only_obs=TRUE)
res$pk1cmt_oral_code <- sim_ode(ode=pk1cmt_oral_code, par=p, regimen=regimen, t_obs=t_obs, only_obs=TRUE)
res$pk1cmt_oral_anal <- pk1cmt_oral_anal(t_obs, dose, p$KA, p$V, p$CL)

## basic testing
assert('library C++ ODE equals analytical function',
       all.equal(round(res$pk1cmt_oral_lib$y, 3), round(res$pk1cmt_oral_anal, 3)))
assert('custom C++ ODE equals analytical function',
       all.equal(round(res$pk1cmt_oral_code$y, 3), round(res$pk1cmt_oral_anal, 3)))

## test precision in time axis
library(PKPDsim)
regimen_mult <- new_regimen(amt=rep(12.8, 3),
                            times = c(0, 6, 12), type="infusion", t_inf = 2)
pk1cmt_iv <- new_ode_model("pk_1cmt_iv")
t_obs <- c(11.916, 14.000, 16.000, 17.000, 30)
tmp <- sim_ode(ode = pk1cmt_iv,
                          par = list(CL = 5, V = 50),
                          regimen = regimen_mult,
                          int_step_size = 1,
                          t_obs = t_obs,
                          only_obs = TRUE)
assert('correct number of observations returned (bug precision time-axis)',
       tmp$t == t_obs)

## test bug EmCo 20150925
xtim<-c(0,2,4,8,12,24)
sujdos<-320
param<-list(KA=1.8, V=30, CL=1.7)
pk1 <- new_ode_model("pk_1cmt_oral")
regim<-new_regimen(amt=sujdos, times=c(0,12))
out<-sim_ode(ode="pk1", par=param, regimen=regim, t_obs = xtim, only_obs = TRUE)
assert("all requested observations in ouput",
       out$t == xtim)

## tests for bug wrong model size (JeHi 20151204)
pk3cmt <- new_ode_model(code = "
                     dAdt[1] = -KA*A[1];
                     dAdt[2] = KA*A[1] -(Q/V)*A[2] + (Q/V2)*A[3] -(CL/V)*A[2];
                     dAdt[3] = -(Q/V2)*A[3] + (Q/V)*A[2];
                     ", obs = list (cmt = 2, scale = "V"))
assert("3cmt model has state vector of 3", attr(pk3cmt, "size") == 3)

## dose in  cmt <> 1, set using model
p <- list(CL = 1, V  = 10, KA = 0.5, S2=.1)
pk  <- new_ode_model(code = "
                     dAdt[1] = -KA * A[1];
                     dAdt[2] = KA*A[1] -(CL/V) * A[2]
                     dAdt[3] = S2*(A[2]-A[3])
                     ",
                     obs = list(cmt=2, scale="V"),
                     dose = list(cmt = 2), cpp_show_code = TRUE)
r <- new_regimen(amt = 100, times = c(0), type = "infusion")
dat <- sim_ode (ode = "pk", n_ind = 1,
                omega = cv_to_omega(par_cv = list("CL"=0.1, "V"=0.1, "KA" = .1), p),
                par = p, regimen = r,
                verbose = FALSE, t_max=48)
assert("dose in comp 2", sum(dat[dat$comp == 1,]$y) == 0 && sum(dat[dat$comp == 2,]$y) > 0)

## dose in  cmt <> 1, set using regimen
r <- new_regimen(amt = c(100, 100, 100),
                 times = c(0, 6, 12),
                 cmt = c(1,2,3),
                 type = "bolus")
dat2 <- sim_ode (ode = "pk", n_ind = 1,
                par = p, regimen = r,
                t_obs = seq(from=0, to=20, by = .1),
                verbose = FALSE, t_max=48)
assert("dose in comp 2 and 3 as well", max(diff(dat2[dat2$comp == 2,]$y)) > 95 && max(diff(dat2[dat2$comp == 2,]$y)) > 95)


## allow infusions from all compartemnts too
r <- new_regimen(amt = c(100, 100, 100),
                 times = c(0, 6, 12),
                 cmt = c(1,2,3), t_inf = 3,
                 type = "infusion")
dat3 <- sim_ode (ode = "pk", n_ind = 1,
                 par = p, regimen = r,
                 t_obs = seq(from=0, to=20, by = .1),
                 verbose = FALSE, t_max=48)
assert("dose in comp 2 and 3 as well", ((max(dat3[dat3$comp == 2,]$y)-142.4)/142.4 < 0.01) && ((max(dat3[dat3$comp == 3,]$y)-157.2)/157.2) < 0.01)


## test duplicate obs (e.g. for optimal design purposes)
p <- list(CL = 1, V  = 10, KA = 0.5, S2=.1)
r <- new_regimen(amt = c(100, 100, 100, 100),
                 times = c(0, 6, 12, 18),
                 cmt = c(2, 2, 1, 1),
                 t_inf = c(1, 1, 1, 1), # for first 2 doses, infusion time will just be ignored, but a value has to be specified in the vector
                 type = c("bolus", "bolus", "infusion", "infusion"))
dat <- sim_ode (ode = pk1cmt_oral_lib, n_ind = 1,
                omega = cv_to_omega(par_cv = list("CL"=0.1, "V"=0.1, "KA" = .1), p),
                par = p, regimen = r,
                t_obs = c(1,2,3,4,4,4,6),
                duplicate_t_obs = T,
                only_obs = F)
assert("some observations duplicated", length(dat[dat$t == 4,]$y) == 9)
assert("all observations included", length(dat$y) == 21)
assert("no NA", any(is.na(dat$y)) == FALSE)
