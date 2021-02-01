library(testit)
library(PKPDsim)

# basic testing
reg <- new_regimen(amt=100, n=4, interval=4)
assert("regimen class", all(c("regimen", "list") %in% class(reg)))

## test type argument
reg_oral <- new_regimen(amt=100, n=4, interval=4, type = "oral")
reg_bolus <- new_regimen(amt=100, n=4, interval=4, type = "bolus")
reg_inf <- new_regimen(amt=100, n=4, interval=4, type = "infusion")
reg_mixed <- new_regimen(amt=100, n=3, interval=4, type = c("bolus", "infusion", "oral"))
assert("oral", all(reg_oral$type == "oral"))
assert("bolus", all(reg_bolus$type == "bolus"))
assert("infusion", all(reg_inf$type == "infusion"))
assert("mixed", all(reg_mixed$type == c("bolus", "infusion", "oral")))

# test auto-detect infusion vs bolues
reg1 <- new_regimen(amt=100, n=4, interval=4, t_inf=0)
reg2 <- new_regimen(amt=100, n=4, interval=4, t_inf=1)
reg3 <- new_regimen(amt=100, n=4, interval=4, t_inf=c(0, 1, 0, 1))
assert("t_inf = 0: bolus", all(reg1$type == "bolus"))
assert("t_inf = 1: infusion", all(reg2$type == "infusion"))
assert("t_inf = mixed: mixed", all(reg3$type == c("bolus", "infusion", "bolus", "infusion")))
