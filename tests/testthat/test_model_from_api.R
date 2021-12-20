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

test_that("Can install a package from json", {
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
  if ("test1cmtiv" %in% installed.packages(lib.loc = instloc)){
    suppressMessages(remove.packages("test1cmtiv", lib = c("", instloc)))
  }
})


