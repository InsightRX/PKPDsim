test_that("Returns vector of available models", {
  mods <- available_default_literature_models()
  expect_type(mods, "character")
  expect_true("pk_busulfan_mccune" %in% mods)
})

test_that("Install default model helper function dispatches correctly", {
  local_mocked_bindings(
    model_from_api = function(...) message("installed model")
  )
  expect_error(install_default_literature_model("an_unknown_model"))
  expect_message(
    install_default_literature_model("pk_busulfan_mccune"),
    "installed model"
  )
  calls <- 0
  local_mocked_bindings(
    model_from_api = function(x, ...) calls <<- calls + 1
  )
  local_mocked_bindings(
    available_default_literature_models = function() c("m1", "m2", "m3")
  )
  # install fake models
  install_default_literature_model("all")
  # mocked function should have been called 3 times
  expect_equal(calls, 3)
})
