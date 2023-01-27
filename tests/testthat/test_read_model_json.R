test_that("read_model_json can read JSON and escape characters", {
  dat <- read_model_json(test_path("sample_json", "test_1cmt_iv.json5"))
  expect_equal(dat$equations, "\n\nfrac{dA_1}{dt} = -(CL/V) \n\ncdot A_1")
  expect_equal(dat$ode_code, "\n    dAdt[0] = -(CL/V)*A[0];")
})
