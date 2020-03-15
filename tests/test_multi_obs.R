## Example multiple observation types (e.g. different compartments! not just different residual errors)library(testit)
library(testit)
library(PKPDsim)
Sys.setenv("R_TESTS" = "")

## define parameters
vars <- c("CONC", "METAB", "METAB2")
pk1 <- new_ode_model(code = "dAdt[1] = -(CL/V)*A[1]; CONC = 1000*A[1]/V; METAB = CONC/2; METAB2 = CONC * t;",
                     obs = list(variable = vars, scale = 1),
                     declare_variables = vars,
                     cpp_show_code = F)

regimen  <- new_regimen(amt = 100, interval = 12, n = 5, type="infusion", t_inf = 1)
parameters   <- list("CL" = 15, "V" = 150)
omega <- PKPDsim::cv_to_omega(list("CL" = 0.2, "V" = 0.2), parameters[1:2])

## simulate single individual in population
obs_type <- c(1,2,1,3,1)
data <- sim(ode = pk1,
            parameters = list(CL = 20, V = 200),
            regimen = regimen,
            int_step_size = 0.1,
            only_obs = TRUE,
            obs_type = obs_type,
            t_obs = c(2, 4, 6, 8, 12),
            output_include = list("variables" = TRUE))

testit::assert("obs_type in data", all(data$obs_type == obs_type))
testit::assert("y taken from correct variables", all(data$y == diag(as.matrix(data[1:5,5+obs_type]))))

## check that obs at same timepoint but with different obs_type get handled properly
t_same <- sim(ode = pk1,
            parameters = list(CL = 20, V = 200),
            regimen = regimen,
            int_step_size = 0.1,
            only_obs = TRUE,
            obs_type = obs_type,
            t_obs = c(2, 4, 4, 8, 8),
            output_include = list("variables" = TRUE))
testit::assert("same t", t_same$t[2] == t_same$t[3] && t_same$t[4] == t_same$t[5])
testit::assert("y correctly outputted", t_same$y[2] == t_same$METAB[2] && t_same$y[3] == t_same$CONC[3])
testit::assert("y correctly outputted", t_same$y[4] == t_same$METAB2[4] && t_same$y[5] == t_same$CONC[5])

## check that residual error correctly applied to right var
set.seed(12345)
ruv_multi <- list(prop = c(0, 0, 0.1), add = c(0, 0, 0.1))
data2 <- sim(ode = pk1,
            parameters = list(CL = 20, V = 200),
            regimen = regimen,
            int_step_size = 0.1,
            only_obs = TRUE,
            obs_type = obs_type,
            t_obs = c(2, 4, 6, 8, 12),
            output_include = list("variables" = TRUE),
            res_var = ruv_multi)
y <- diag(as.matrix(data2[1:5,5+obs_type]))
testit::assert("obs_type 1 and 2 get no residual error", all(data2$y[-4] == y[-4]))
testit::assert("only obs_type 3 gets residual error", data2$y[-4][4] != y[4])

## check that residual error correctly applied to right var
set.seed(12345)
ruv_multi <- list(prop = c(.1, 0, 0), add = c(1, 0, 0))
data3 <- sim(ode = pk1,
             parameters = list(CL = 20, V = 200),
             regimen = regimen,
             int_step_size = 0.1,
             only_obs = TRUE,
             obs_type = obs_type,
             t_obs = c(2, 4, 6, 8, 12),
             output_include = list("variables" = TRUE),
             res_var = ruv_multi)
y <- diag(as.matrix(data3[1:5,5+obs_type]))
testit::assert("obs_type 2 and 3 get no residual error", all(data3$y[-c(1,3,5)] == y[-c(1,3,5)]))
testit::assert("obs_type 1 gets residual error", data3$y[c(1,3,5)] != y[c(1,3,5)])

