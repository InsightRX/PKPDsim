library(testit)
library(PKPDsim)

## test if remove_negative_times works properly
cov1 <- new_covariate(value = c(1,2,3,4), times = c(-10, -5, 0.5, 3),
              implementation = "interpolate", remove_negative_times = TRUE)
cov2 <- new_covariate(value = c(1,2,3,4), times = c(-10, -5, 0.5, 3),
              implementation = "locf", remove_negative_times = TRUE)
cov3 <- new_covariate(value = c(1,2,3,4), times = c(-10, -5, 0.5, 3),
              implementation = "interpolate", remove_negative_times = FALSE)
cov4 <- new_covariate(value = c(0.8, 1.1), times=c(-0.0167, 0.5))

assert(all(round(cov1$value,1) == c(2.9, 3, 4)))
assert(all(round(cov2$value,1) == c(2, 3, 4)))
assert(all(round(cov3$value,1) == c(1, 2, 3, 4)))
assert(!any(is.na(cov4$value)))

## check that units are respected when handling covarites at t < 0 and t[1] >0.
cov_unit1 <- new_covariate(
    value = c(20, 10, 10, 20),
    times = c(-3, 1, 5, 10),
    unit = c("lbs", "kg", "kg", "lbs"))
assert(all(cov_unit1$unit == c("lbs", "kg", "kg", "lbs")))
cov_unit2 <- new_covariate(
    value = c(20, 10, 10, 20),
    times = c(1, 3, 5, 10),
    unit = c("lbs", "kg", "kg", "lbs"))
assert(all(cov_unit2$unit == c("lbs", "kg", "kg", "lbs")))

## Joining regimens:
tmp_join <- new_covariate(value = c(1,2,3,4,5), times=c(0,.3,.5,1.7,1.9))
assert("joining works", length(tmp_join$times) == 2 && length(tmp_join$value) == 2)


