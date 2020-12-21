library(testit)
library(PKPDsim)
Sys.setenv("R_TESTS" = "")

## Infusion regimens
keys <- c("oral", "infusion", "bolus")
for(key in keys) {
  reg1 <- new_regimen(amt = 2000, interval = 24, n = 6, type = key, t_inf = 1)
  reg1_s1 <- pop_regimen(reg1) # without n
  reg1_s2 <- pop_regimen(reg1, n = 4) # with specific n
  assert("Still a valid PKPDsim regimen", "regimen" %in% class(reg1_s1))
  assert("Correct number of doses when shifted 1", length(reg1_s1$dose_amts) == 5)
  assert("Correct number of doses when shifted 4", length(reg1_s2$dose_amts) == 2)
  assert("Correct number of t_inf when shifted 4", length(reg1_s2$t_inf) == 2)

  reg2 <- new_regimen(amt = c(1:6), times=c(0:5)*24, type = "infusion")
  reg2_s1 <- pop_regimen(reg2, n=3)
  assert("Amts are taken from end, not fron", reg2_s1$dose_amts == c(1,2,3))
  assert("Still a valid PKPDsim regimen", "regimen" %in% class(reg1_s1))
  reg2_s1 <- pop_regimen(reg2, n=10)
  assert("NULL when no more doses", is.null(reg2_s1))
}
