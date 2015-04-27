#' @export
  pk_oral <- PKPDsim::new_ode_model(code = "
    dAdt[1] = -KA*A[1];
    dAdt[2] = KA*A[1] - (CL/V)*A[2];
  ")
