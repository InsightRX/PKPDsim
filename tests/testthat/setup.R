mod_1cmt_iv <- new_ode_model("pk_1cmt_iv")
mod_2cmt_iv <- new_ode_model("pk_2cmt_iv")
mod_1cmt_oral <- new_ode_model("pk_1cmt_oral")
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
