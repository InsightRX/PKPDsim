library(PKPDsim)
library(testit)

Sys.setenv("R_TESTS" = "")


pk1cmt_oral_code <- new_ode_model(code = "
                                  dAdt[1] = -KA*A[1];
                                  dAdt[2] = KA*A[1] - (CL/V)*A[2];",
                                  obs = list(cmt = 2, scale="V"),
                                  cmt_mapping = list(oral = 1, infusion = 2, bolus = 2))
regimen <- new_regimen(amt = c(100, 100, 100, 100),
                       times = c(0, 12, 24, 36),
                       type = c("oral", "oral", "infusion", "infusion"),
                       t_inf = c(0, 0, 1, 1))
p <- list(KA = 1, CL = 5, V = 50)
res <-  sim_ode(ode = pk1cmt_oral_code,
                parameters = p,
                regimen = regimen,
                only_obs = FALSE)
assert("cmt_mapping added to attr", attr(pk1cmt_oral_code, "cmt_mapping")[["oral"]] == 1 &
         attr(pk1cmt_oral_code, "cmt_mapping")[["infusion"]] == 2)
assert("admin route is interpreted and simulated correctly",
       round(res$y[res$comp == 1 & res$t == 25], 4) == 2e-04 &&
         round(res$y[res$comp == 2 & res$t == 25], 1) >= 100)
