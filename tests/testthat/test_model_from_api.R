test_that("Can specify a package from json", {
  mod <- model_from_api(
    model = "test_1cmt_iv",
    url = test_path("sample_json", "test_1cmt_iv.json5"),
    to_package = FALSE,
    verbose = FALSE,
    force = TRUE
  )
  expect_true("PKPDsim" %in% class(mod))
})

test_that("Can install a package from json, and spec added to package", {
  skip_on_cran()
  # This test specifies tmp directories explicitly during install.
  # Environment settings for tmp directories change from system to system
  # and so this approach ensures consistency.
  instloc <- tempfile("package")
  on.exit(add = TRUE, {
    unlink(instloc, recursive = TRUE)
  })

  copyloc <- paste0(instloc, "/def")
  dir.create(instloc)
  dir.create(copyloc)

  if ("test1cmtiv" %in% installed.packages(lib.loc = instloc)) {
    suppressMessages(remove.packages("test1cmtiv", lib.loc = instloc))
  }

  suppressWarnings(
    suppressMessages(
      mod <- model_from_api(
        model = "test_1cmt_iv",
        url = test_path("sample_json", "test_1cmt_iv.json5"),
        lib_location = instloc,
        folder = copyloc,
        to_package = TRUE,
        verbose = FALSE,
        quiet = NULL
      )
    )
  )

  expect_true("test1cmtiv" %in% installed.packages(lib.loc = instloc))
  suppressMessages(require(test1cmtiv, lib.loc = instloc))
  mod_from_pkg <- test1cmtiv::model()
  expect_true("PKPDsim" %in% class(mod_from_pkg))
  expect_true(
    file.exists(file.path(instloc, "test1cmtiv", "definition.json5"))
  )
  if ("test1cmtiv" %in% installed.packages(lib.loc = instloc)){
    suppressMessages(remove.packages("test1cmtiv", lib = c("", instloc)))
  }

})


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
