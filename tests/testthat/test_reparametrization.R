par_orig <- list(
  V = 25.76,
  SCLSlope = 0.036,
  K12 = 2.29,
  K21 = 1.44,
  SCLInter = 0.18,
  TDM_INIT = 0)

covs <- list(
  CRCL = PKPDsim::new_covariate(5),
  CL_HEMO = PKPDsim::new_covariate(0)
)
model <- new_ode_model( # Carreno et al
  code = "
    CLi = SCLInter + SCLSlope * (CRCL*16.667) + CL_HEMO \
    Vi = V \
    Qi = K12 * Vi \
    V2i = Qi / K21 \
    dAdt[0] = -(CLi/V)*A[0] - K12*A[0] + K21*A[1] \
    dAdt[1] = K12*A[0] - K21*A[1] \
    dAdt[2] = A[0]/V
  ",
  parameters = par_orig,
  declare_variables = c("CLi", "Qi", "Vi", "V2i"),
  covariates= covs,
  obs = list(cmt = 1, scale = "V"),
  reparametrization = list(
    "CL" = "SCLInter + SCLSlope * (CRCL*16.667) + CL_HEMO",
    "V" = "V",
    "Q" = "K12 * V",
    "V2" = "(K12 * V) / K21"
  )
)
pars_covs_comb <- join_cov_and_par(covs, par_orig)
pars <- reparametrize(model, pars_covs_comb, covariates = covs)

reg <- new_regimen(
  amt = 1000,
  interval = 24,
  t_inf = 1,
  n = 1,
  type = "infusion",
  ss = TRUE,
  n_ss = 50
)
s <- sim(
  ode = model,
  parameters = par_orig,
  regimen = reg,
  covariates = covs,
  t_obs = c(0,24)
)

test_that("ODE-integrated model and reparametrized model match AUC", {
  dose <- 1000
  auc_ss_lin <- dose / pars$CL
  auc_ss_ode <- diff(s[s$comp == 3,]$y)

  expect_equal(auc_ss_ode, auc_ss_lin)
})

test_that("Reparametrized model and analytics equations match", {
  pk_2cmt_inf_ss <- function ( # (taken from clinPK)
    t = c(0:24),
    dose = 100,
    t_inf = 1,
    tau = 12,
    CL = 3,
    V = 30,
    Q = 2,
    V2 = 20
  ) {
    k <- CL/V
    tmp <- c()
    t_dos <- t%%tau
    terms <- (Q/V) + (Q/V2) + (CL/V)
    beta <- 0.5 * (terms - sqrt(terms^2 - 4 * (Q/V2) * (CL/V)))
    alpha <- ((Q/V2) * (CL/V))/beta
    A <- (1/V) * (alpha - (Q/V2))/(alpha - beta)
    B <- (1/V) * ((beta - Q/V2)/(beta - alpha))
    dat <- data.frame(cbind(t = t, dv = 0))
    dat$dv[t_dos <= t_inf] <- (dose/t_inf) * ((A/alpha) * ((1 - exp(-alpha * t_dos[t_dos <= t_inf])) + exp(-alpha * tau) * ((1 - exp(-alpha * t_inf)) * exp(-alpha * (t_dos[t_dos <= t_inf] - t_inf))/(1 - exp(-alpha * tau)))) + (B/beta) * ((1 - exp(-beta * t_dos[t_dos <= t_inf])) + exp(-beta * tau) * ((1 - exp(-beta * t_inf)) * exp(-beta * (t_dos[t_dos <= t_inf] - t_inf))/(1 - exp(-beta * tau)))))
    dat$dv[t_dos > t_inf] <- (dose/t_inf) * (((A/alpha) * (1 - exp(-alpha * t_inf)) * exp(-alpha * (t_dos[t_dos > t_inf] - t_inf))/(1 - exp(-alpha * tau))) + ((B/beta) * (1 - exp(-beta * t_inf)) * exp(-beta * (t_dos[t_dos > t_inf] - t_inf))/(1 - exp(-beta * tau))))
    dat
  }
  cmin_ss_lin <- pk_2cmt_inf_ss(
    t = c(24),
    dose = 1000,
    t_inf = 1,
    tau = 24,
    CL = pars$CL,
    V = pars$V,
    Q = pars$Q,
    V2 = pars$V2
  )$dv
  cmin_ss_ode <- s[s$comp == "obs",]$y[1]
  expect_equal(cmin_ss_ode, cmin_ss_lin)
})

