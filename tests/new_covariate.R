library(testit)
library(PKPDsim)

## test if remove_negative_times works properly
cov1 <- new_covariate(value = c(1,2,3,4), times = c(-10, -5, 0.5, 3),
              implementation = "interpolate", remove_negative_times = TRUE)
cov2 <- new_covariate(value = c(1,2,3,4), times = c(-10, -5, 0.5, 3),
              implementation = "locf", remove_negative_times = TRUE)
cov3 <- new_covariate(value = c(1,2,3,4), times = c(-10, -5, 0.5, 3),
              implementation = "interpolate", remove_negative_times = FALSE)

assert(all(round(cov1$value,1) == c(2.9, 3, 4)))
assert(all(round(cov2$value,1) == c(2, 3, 4)))
assert(all(round(cov3$value,1) == c(1, 2, 3, 4)))
