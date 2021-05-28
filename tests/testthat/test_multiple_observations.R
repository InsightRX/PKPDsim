library(PKPDsim)
library(testit)
Sys.setenv("R_TESTS" = "")

mod <- new_ode_model(code = "
  dAdt[1] = -KA * A[1];
  dAdt[2] = -(CL/V) * A[2] + KA*A[1];
", obs = list(cmt = c(2, 2),
              scale = c(1, "V"),
              label = c("abs", "conc")), cpp_show_code = F)

assert("PKPDsim" %in% class(mod))

par <- list(CL = 5, V = 50, KA = .5)
reg <- new_regimen(amt = 100, n = 5, interval = 12)
res <- sim_ode(ode = mod, parameters = par, regimen = reg, only_obs=T)

assert(length(unique(res$comp)) == 2)
dat <- cbind(res[res$comp == "abs",]$y, res[res$comp == "conc",]$y)
assert(all(round(dat[,1],1) == round(dat[,2]*par$V,1)))
