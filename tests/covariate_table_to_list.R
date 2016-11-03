## covariate_table_to_list
library(PKPDsim)
library(testit)

cov_table <- data.frame(id=c(1,1,2,3), WT = c(40, 45, 50, 60), SCR = c(50, 150, 90,110), t = c(0, 5, 0, 0))
test1 <- covariate_table_to_list(cov_table)
assert(length(test1) == 3)
assert(test1[[1]]$WT$value == c(40,45))
assert(test1[[2]]$WT$value == 50)
assert(test1[[1]]$SCR$implementation == "interpolate")

test2 <- covariate_table_to_list(cov_table, list(SCR = "locf"))
assert(test2[[1]]$SCR$implementation == "locf")
