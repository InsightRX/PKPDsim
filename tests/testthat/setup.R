# ---- Library Models ----
mod_1cmt_iv <- new_ode_model("pk_1cmt_iv")
mod_2cmt_iv <- new_ode_model("pk_2cmt_iv")
mod_1cmt_oral <- new_ode_model("pk_1cmt_oral")

# ---- Shared ODE Models ----
# 1-cmt oral with explicit code (used in compare tests)
mod_1cmt_oral_code <- new_ode_model(
  code = "dAdt[1] = -KA*A[1]; dAdt[2] = KA*A[1] - (CL/V)*A[2];",
  obs = list(cmt = 2, scale = "V")
)

# Model with dose in compartment 2 (used in compare tests)
mod_dose_cmt_2 <- new_ode_model(
  code = "
    dAdt[1] = -KA * A[1];
    dAdt[2] = KA*A[1] -(CL/V) * A[2]
    dAdt[3] = S2*(A[2]-A[3])
  ",
  obs = list(cmt = 2, scale = "V"),
  dose = list(cmt = 2),
  cpp_show_code = FALSE
)

# Basic 1-cmt without IOV (used in IOV tests)
mod_1cmt_no_iov <- new_ode_model(
  code = "
    dAdt[1] = -KA * A[1]
    dAdt[2] = +KA * A[1] -(CL/V) * A[2]
  ",
  obs = list(cmt = 2, scale = "V"),
  dose = list(cmt = 1, bioav = 1),
  parameters = c("CL", "V", "KA"),
  cpp_show_code = FALSE
)

# 1-cmt with IOV structure (used in IOV tests)
mod_1cmt_iov <- new_ode_model(
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
  parameters = c("kappa_CL_1", "kappa_CL_2", "kappa_CL_3", "eta_CL", "CL", "V", "KA"),
  cpp_show_code = FALSE
)

# ---- NOT_CRAN Models (for ADVAN comparison tests) ----
if (identical(Sys.getenv("NOT_CRAN"), "true")) {
  # Standard parameters for ADVAN tests
  advan_parameters <- list(
    CL = 10, V = 50, KA = 0.5,
    Q = 5, V2 = 100,
    Q2 = 3, V3 = 150,
    F1 = 1
  )

  # 1-cmt IV with AUC compartment
  mod_1cmt_iv_auc <- new_ode_model(
    code = "dAdt[1] = -(CL/V)*A[1]; dAdt[2] = A[1]/V;",
    parameters = advan_parameters
  )

  # 2-cmt IV with AUC compartment
  mod_2cmt_iv_auc <- new_ode_model(
    code = "
      dAdt[1] = -(CL/V)*A[1] - (Q/V)*A[1] + (Q/V2)*A[2];
      dAdt[2] = +(Q/V)*A[1] - (Q/V2)*A[2];
      dAdt[3] = A[1]/V;
    ",
    parameters = advan_parameters
  )

  # 3-cmt IV with AUC compartment
  mod_3cmt_iv_auc <- new_ode_model(
    code = "
      dAdt[1] = -(CL/V)*A[1] - (Q/V)*A[1] + (Q/V2)*A[2] - (Q2/V)*A[1] + (Q2/V3)*A[3];
      dAdt[2] =                (Q/V)*A[1]  -(Q/V2)*A[2]                             ;
      dAdt[3] =                                           (Q2/V)*A[1] - (Q2/V3)*A[3];
      dAdt[4] = A[1]/V;
    ",
    parameters = advan_parameters
  )
}
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
oral_1cmt_allometric <- new_ode_model( # also timevarying and dose-dependence factor
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
