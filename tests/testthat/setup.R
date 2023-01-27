mod_1cmt_iv <- new_ode_model("pk_1cmt_iv")
pars <- list(CL = 5, V = 50)
mod_1cmt_iv_auc <- new_ode_model(code =
  "dAdt[1] = -(CL/V)*A[1]\
  dAdt[2] = (A[1]/V)
  ",
  parameters= pars,
  obs = list(cmt = 1, scale = "V")
)


mod_2cmt_iv <- new_ode_model("pk_2cmt_iv")
mod_1cmt_oral <- new_ode_model("pk_1cmt_oral")
oral_1cmt_allometric <- new_ode_model(
  code = "
      CLi = CL * pow(WT/70, 0.75)
      dAdt[1] = -KA * A[1]
      dAdt[2] = KA*A[1] - (CLi/V)*A[2]
    ",
  dose = list(cmt = 1, bioav = 1),
  covariates = list(WT = new_covariate(50)),
  declare_variables = "CLi",
  parameters = list(KA = 0.5, CL = 5, V = 50)
)
