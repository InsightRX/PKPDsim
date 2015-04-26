# benchmark script from Devin Pastoor

library(ggplot2)
install.packages("microbenchmark")
library(devtools)
# install_github("ronkeizer/PKPDsim")
# install_github("MarcLavielle/mlxR")
library(PKPDsim)
library(mlxR)
library(microbenchmark)
library(Rcpp)
library(BH)

setwd("~/git/PKPDsim/R")

p <- list(CL = 5,
          V  = 50,
          KA = .5)

r1 <- new_regimen(amt = 100,
                  times = c(0, 12, 24, 36, 48))

## run once to compile function
sim_ode (ode = "pk_1cmt_oral",
         par = p,
         regimen = r1,
         step_size = 1,
         t_max = 72,
         cpp = TRUE, cpp_recompile=TRUE)

sim_ode_1000 <- function() {
  sim_ode (ode = "pk_1cmt_oral",
           par = p,
           n_ind = 10000,
           regimen = r1,
           step_size = 1,
           t_max = 48,
           cpp = TRUE, cpp_recompile = FALSE)
}
microbenchmark(sim_ode_1000(), times = 2L)

## mlxR


adm <- list(time=seq(0,to=48,by=12), amount=100)
ppk <- c(ka_pop=0.5,   V_pop=50,    k_pop=0.1,
         omega_ka=0.1, omega_V=0.5, omega_k=0.1, a1=0.05)
g   <- list(size=10000, level='individual')
Cc  <- list(name='Cc', time=seq(0,to=48,by=1))
y1  <- list(name='y1', time=seq(0,to=48,by=1))
s   <- list(seed=1234)

res1 <- simulx(model     = "examples/mlxtest.txt",
               parameter = ppk,
               treatment = adm,
               group     = g,
               output    = list(y1),
               settings  = s)

mlx_1000 <- function() {
  res1 <- simulx(model     = "examples/mlxtest.txt",
                 parameter = ppk,
                 treatment = adm,
                 group     = g,
                 output    = list(y1),
                 settings  = s)
}
microbenchmark(mlx_1000(), times = 2L)
