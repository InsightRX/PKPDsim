library(testit)
library(PKPDsim)

tmp_join <- new_covariate(value = c(1,2,3,4,5), times=c(0,.3,.5,1.7,1.9))
assert("joining works", length(tmp_join$times) == 2 && length(tmp_join$value) == 2)
