# =============================================================================
# Shared models compiled once at test session startup
# =============================================================================

# --- Library models (standard PK models) ---
mod_1cmt_iv <- new_ode_model("pk_1cmt_iv")
mod_2cmt_iv <- new_ode_model("pk_2cmt_iv")
mod_1cmt_oral <- new_ode_model("pk_1cmt_oral")

# --- Custom 1cmt oral with lagtime ---
mod_1cmt_oral_lagtime <- new_ode_model(
  code = "
    dAdt[0] = -KA * A[0]
    dAdt[1] = +KA * A[0] -(CL/V) * A[1]
  ",
  lagtime = c("TLAG", 0),
  obs = list(cmt = 2, scale = "V"),
  dose = list(cmt = 1, bioav = 1),
  parameters = list(CL = 5, V = 50, KA = 0.5, TLAG = 0.83)
)

# --- 1cmt oral with allometric scaling, time-varying CL, dose-dependent V ---
oral_1cmt_allometric <- new_ode_model(
  code = "
      if(t<168.0) {
        CLi = CL * pow(WT/70, 0.75)
      } else {
        CLi = CL * pow(WT/70, 0.75) * 1.5
      }
      if(prv_dose > 1000.0) {
        Vi = V * 2.0
      } else {
        Vi = V
      }
      dAdt[1] = -KA * A[1]
      dAdt[2] = KA*A[1] - (CLi/Vi)*A[2]
      dAdt[3] = A[2]/Vi
    ",
  dose = list(cmt = 1, bioav = 1),
  covariates = list(WT = new_covariate(50)),
  declare_variables = c("CLi", "Vi"),
  parameters = list(KA = 0.5, CL = 5, V = 50)
)

# --- From test_iov.R: IOV models ---
pars_iov <- list(
  "kappa_CL_1" = 0,
  "kappa_CL_2" = 0,
  "kappa_CL_3" = 0,
  "eta_CL" = 0,
  "CL" = 5,
  "V" = 50,
  "KA" = 1
)
pars_iov_no_iov <- list(
  "CL" = 5,
  "V" = 50,
  "KA" = 1
)
pk_iov_none <- new_ode_model(
  code = "
      dAdt[1] = -KA * A[1]
      dAdt[2] = +KA * A[1] -(CL/V) * A[2]
    ",
  obs = list(cmt = 2, scale = "V"),
  dose = list(cmt = 1, bioav = 1),
  parameters = names(pars_iov_no_iov),
  cpp_show_code = FALSE
)
pk_iov <- new_ode_model(
  code = "
      CL_iov = CL * exp(kappa_CL + eta_CL);
      dAdt[1] = -KA * A[1]
      dAdt[2] = +KA * A[1] -(CL_iov/V) * A[2]
    ",
  iov = list(
    cv = list(CL = 0.2),
    n_bins = 3
  ),
  obs = list(cmt = 2, scale = "V"),
  dose = list(cmt = 1, bioav = 1),
  declare_variables = c("kappa_CL", "CL_iov"),
  parameters = names(pars_iov),
  cpp_show_code = FALSE
)

# --- From test_compare_results.R ---
dose_in_cmt_2 <- new_ode_model(
  code = "
      dAdt[1] = -KA * A[1];
      dAdt[2] = KA*A[1] -(CL/V) * A[2]
      dAdt[3] = S2*(A[2]-A[3])
    ",
  obs = list(cmt = 2, scale = "V"),
  dose = list(cmt = 2),
  cpp_show_code = FALSE
)

# --- From test_multi_obs.R: Multi-observation model ---
vars_multi_obs <- c("CONC", "METAB", "METAB2", "ACT")
pk_multi_obs <- new_ode_model(
  code = "dAdt[1] = -(CL/V)*A[1]; CONC = 1000*A[1]/V; METAB = CONC/2; METAB2 = CONC * t; ACT = 15",
  obs = list(variable = vars_multi_obs),
  declare_variables = vars_multi_obs,
  cpp_show_code = FALSE
)

# --- From test_mixture_model.R ---
covs_mixture <- list(WT = new_covariate(70))
mod_mixture <- new_ode_model(
  code = "
      dAdt[0] = -(CL*(WT/70.0)/V)*A[0];
    ",
  pk_code = " ",
  obs = list(cmt = 1, scale = "V"),
  mixture = list(CL = list(values = c(5, 15), probability = 0.3)),
  covariates = covs_mixture
)

# --- Conditional models (skip on CRAN due to compilation time) ---
if (identical(Sys.getenv("NOT_CRAN"), "true")) {

  # Library models
  pk_3cmt_iv <- new_ode_model("pk_3cmt_iv")
  pk_2cmt_oral <- new_ode_model("pk_2cmt_oral")
  pk_3cmt_oral <- new_ode_model("pk_3cmt_oral")
  mod_1cmt_iv_mm <- new_ode_model("pk_1cmt_iv_mm")

  # --- From test_advan_with_auc.R: Models with AUC compartments ---
  parameters_advan_auc <- list(
    CL = 10, V = 50, KA = 0.5, Q = 5, V2 = 100, Q2 = 3, V3 = 150, F1 = 1
  )
  mod_1cmt_auc <- new_ode_model(
    code = "dAdt[1] = -(CL/V)*A[1]; dAdt[2] = A[1]/V;",
    parameters = parameters_advan_auc
  )
  mod_2cmt_auc <- new_ode_model(
    code = "
    dAdt[1] = -(CL/V)*A[1] - (Q/V)*A[1] + (Q/V2)*A[2];
    dAdt[2] = +(Q/V)*A[1] - (Q/V2)*A[2];
    dAdt[3] = A[1]/V;
  ",
    parameters = parameters_advan_auc
  )
  mod_3cmt_auc <- new_ode_model(
    code = "
    dAdt[1] = -(CL/V)*A[1] - (Q/V)*A[1] + (Q/V2)*A[2] - (Q2/V)*A[1] + (Q2/V3)*A[3];
    dAdt[2] =                (Q/V)*A[1]  -(Q/V2)*A[2]                             ;
    dAdt[3] =                                           (Q2/V)*A[1] - (Q2/V3)*A[3];
    dAdt[4] = A[1]/V;
  ",
    parameters = parameters_advan_auc
  )

  # --- From test_timevar_cov.R: 2cmt with time-varying covariates ---
  mod_2cmt_timevar <- new_ode_model(
    code = "
      dAdt[1] = -(Q/V)*A[1] + (Q/V2)*A[2] -(CLi/V)*A[1];
      dAdt[2] = -(Q/V2)*A[2] + (Q/V)*A[1];
    ",
    pk_code = "CLi = CL + CRCL",
    obs = list(cmt = 2, scale = "V"),
    covariates = list(CRCL = new_covariate(5)),
    declare_variables = "CLi",
    cpp = FALSE
  )

  # --- From test_t_init.R: Model with state initialization ---
  mod_t_init <- new_ode_model(
    code = "CLi = CL; Vi = V; dAdt[1] = -(CLi/Vi)*A[1]; CONC = A[1]/Vi",
    state_init = "A[1] = TDM_INIT * Vi",
    parameters = list(CL = 7.67, V = 97.7, TDM_INIT = 500),
    obs = list(cmt = 1, scale = "Vi"),
    declare_variables = c("CONC", "CLi", "Vi"),
    cpp_show_code = FALSE
  )

  # --- From test_reparametrization.R: Carreno 2cmt model ---
  covs_carreno <- list(
    CRCL = new_covariate(5),
    CL_HEMO = new_covariate(0)
  )
  model_carreno <- new_ode_model(
    code = "
    CLi = SCLInter + SCLSlope * (CRCL*16.667) + CL_HEMO \
    Vi = V \
    Qi = K12 * Vi \
    V2i = Qi / K21 \
    dAdt[0] = -(CLi/V)*A[0] - K12*A[0] + K21*A[1] \
    dAdt[1] = K12*A[0] - K21*A[1] \
    dAdt[2] = A[0]/V
  ",
    parameters = list(
      V = 25.76, SCLSlope = 0.036, K12 = 2.29, K21 = 1.44,
      SCLInter = 0.18, TDM_INIT = 0
    ),
    declare_variables = c("CLi", "Qi", "Vi", "V2i"),
    covariates = covs_carreno,
    obs = list(cmt = 1, scale = "V"),
    reparametrization = list(
      "CL" = "SCLInter + SCLSlope * (CRCL*16.667) + CL_HEMO",
      "V" = "V",
      "Q" = "K12 * V",
      "V2" = "(K12 * V) / K21"
    )
  )
}
