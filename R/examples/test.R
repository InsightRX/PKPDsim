library(devtools)
install_github("ronkeizer/PKPDsim")
library(PKPDsim)
library(ggplot2)

code <- list(
  efv = "
  double k = CL/V1;
  dAdt[1] = -ka*A[1];
  dAdt[2] = ka*A[1] - k*A[2];
  double conc = A[2]/V1;
  if (conc > 1) {
  dAdt[3] = 1;
  } else {
  dAdt[3] = 0;
  }",
  viral_load = "
  double t_bool = 0;
  if (t/24 > tr) {
  t_bool = 1;
  }
  IC50 = t_bool * Ir + (1-t_bool) * (I0 + ((Ir-I0)/(tr*24))*t);
  double alpha  = conc / (IC50 + conc);
  dAdt[1] = lambda - d_t*A[1] - (1-alpha)*k_inf*A[1]*A[3];
  dAdt[2] = (1-alpha)*k_inf*A[1]*A[3] - delta*A[2] ;
  dAdt[3] = N * delta * A[2] - c*A[3];
  ") # gamma was renamed alpha since gamma is reserved word

mod1 <- new_ode_model(code = code, obs = list(cmt = 6))
mod1 <- new_ode_model(code = code, obs = list(cmt = 2, scale="V1"))
mod1 <- new_ode_model(code = code, obs = list(cmt = c(2, 6), scale=c("V1", 1), labels=c("conc", "viral_load")))

### Parameters
p <- list (CL = 10,
           V1 = 300,
           ka = .65,
           d_t = 1.396e-3 /24,
           k_inf = 0.545e-5 /24,
           c = 9.38 /24,
           delta = 0.215 /24,
           lambda = 195 /24,
           N = 249,
           I0 = 0.0044,
           Ir = 0.44,
           tr = 50,
           IC50 = 1)
r1 <- new_regimen (amt = 400,
                   n = 80,
                   interval = 24)

dat <- sim_ode (ode = "mod1",
         regimen = r1,
         obs_step_size = 2,
         t_max = 180*24,
         parameters = p,
         A_init = c(0,0, 0, 1,1,1))

