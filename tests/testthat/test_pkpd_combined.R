test_that("combined PKPD model compiles", {
  skip_on_cran()
  pkpd <- new_ode_model(
    code = list(
      pk = "dAdt[1] = -(CL/V) * A[1]; conc = A[1]/V;",
      pd = "dAdt[1] = KIN * 1/(1+EFF*conc) - KOUT*A[1];"
    ),
    state_init = list(pd = "A[1] = KIN/KOUT;"),
    cpp_show_code = FALSE
  )
  expect_true("PKPDsim" %in% class(pkpd))
})
