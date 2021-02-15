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

# rounding issues
test_reg <- new_regimen(amt = 1500, interval = 24, times = 145.217, type = "infusion", t_inf = 1.5)
p <- list(CL = 4.5, V = 58.4, V2 = 38.4, Q = 6.5, TH_CRCL = 0.8, TH_DIAL_CL = 0.7,
             TH_DIAL_V = 0.5, TDM_INIT = 17.2)
covs <- list(PMA = structure(list(value = c(3313.24850396581, 3313.39136110867),times = c(0, 24),
                       implementation = "interpolate", unit = c("weeks", "weeks"),
                       comments = NULL), class = c("covariate", "list" )))
t_obs <- c(-145.217, -122.217, -121.217, -120.217, -0.217, 0, 1, 1.5, 23, 24, 25, 168)
res2 <- parse_regimen(test_reg, t_init = 145.217, t_obs = t_obs, covariates = covs)
assert("merge matches times within 6 decimal places", sum(is.na(res2$cov_PMA)) == 0)
