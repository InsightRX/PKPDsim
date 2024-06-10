test_that("Model info can be accessed easily", {
  expect_null(get_model_covariates(mod_1cmt_oral))
  expect_equal(get_model_covariates(oral_1cmt_allometric), "WT")
  expect_equal(get_model_parameters(oral_1cmt_allometric), c("KA", "CL", "V"))
  expect_null(get_model_fixed_parameters(oral_1cmt_allometric))
  expect_equal(get_model_auc_compartment(oral_1cmt_allometric), 3)
  expect_equal(get_model_iov(oral_1cmt_allometric), list(n_bins = 1))
})
