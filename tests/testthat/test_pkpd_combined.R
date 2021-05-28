library(testit)
library(PKPDsim)
Sys.setenv("R_TESTS" = "")

## combined PKPDmodel compiles
pkpd <- new_ode_model(code = list("pk" = "
                                    dAdt[1] = -(CL/V) * A[1];
                                    conc = A[1]/V;
                                  ",
                                  "pd" = "
                                    dAdt[1] = KIN * 1/(1+EFF*conc) - KOUT*A[1];
                                  "),
                      state_init = list(pd = "A[1] = KIN/KOUT;"), cpp_show_code = F)
assert("PKPDsim" %in% class(pkpd))
