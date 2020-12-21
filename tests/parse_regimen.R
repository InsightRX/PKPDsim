library(testit)
library(PKPDsim)
Sys.setenv("R_TESTS" = "")

reg_bolus <- new_regimen(amt = 100,
                         interval = 24,
                         n = 5,
                         type = "bolus")
covs <- list(WT = new_covariate(value = c(80, 100), time = c(0, 50), implementation = "interpolate"))
res <- parse_regimen(reg_bolus,
              covariates = covs,
              t_obs = seq(0, 100, 5))
assert("right number of rows", nrow(res) == 26)
assert("all columns created", all(names(res) == c("t", "dose", "type", "dum", "dose_cmt", "t_inf", "evid", "bioav", "rate", "cov_WT", "cov_t_WT", "gradients_WT", "obs_type")))
assert("gradients parsed correctly", res$gradients_WT[1] == 0.4)
assert("all doses implemented", sum(res$dose) == 500)
