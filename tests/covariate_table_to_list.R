## covariates_table_to_list
library(PKPDsim)
library(testit)

cov_table <- data.frame(id=c(1,1,2,3), WT = c(40, 45, 50, 60), SCR = c(50, 150, 90,110), t = c(0, 5, 0, 0))
test1 <- covariates_table_to_list(cov_table)
assert(length(test1) == 3)
assert(test1[[1]]$WT$value == c(40,45))
assert(test1[[2]]$WT$value == 50)
assert(test1[[1]]$SCR$implementation == "interpolate")

test2 <- covariates_table_to_list(cov_table, list(SCR = "locf"))
assert(test2[[1]]$SCR$implementation == "locf")

p <- list(
  CL = 5,
  V = 50
)
pk1 <- new_ode_model(
  code = "
    dAdt[1] = -(CL/V) * (SCR/80) * A[1]
  ",
  dose = list(cmt = 1, bioav = 1),
  obs = list(cmt = 1, scale = "V"),
  cpp_show_code = F,
  dose_code = "rate[1] = rate[1] / V",
  parameters = p, covariates=c("SCR", "WT"))

reg <- new_regimen (amt = 100, n = 4, interval = 12, type = "bolus",  cmt=1)
om <- cv_to_omega(list(CL = 0.1, V = 0.1), p)
cov_table <- data.frame(id=c(1,1,2,3), WT = c(40, 45, 50, 60), SCR = c(50, 150, 90,110), t = c(0, 5, 0, 0))

dat <- sim(pk1, parameters = p, regimen = reg,
           covariates_table = cov_table,
           covariates_implementation = list(SCR = "interpolate"),
           omega = NULL, n_ind = 3, only_obs = T,
           output_include = list(parameters = TRUE, covariates=TRUE)
)
testit::assert(length(unique(dat$id)) == 3)

