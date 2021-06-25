test_that("Can specify a package from json", {
  mod <- model_from_api(
    model = "test_1cmt_iv",
    url = system.file(package = "PKPDsim"),
    to_package = FALSE,
    verbose = FALSE,
    force = TRUE
  )
  expect_true("PKPDsim" %in% class(mod))
})

test_that("Can install and test a package from json", {
  # model_from_api will stop if test fails/can't be found
  # this test specifies tmp directories explicitly during install
  # environment settings for tmp directories change from system to system
  # and so this approach ensures consistency
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
        url = system.file(package = "PKPDsim"),
        lib_location = instloc,
        folder = copyloc,
        to_package = TRUE,
        verbose = FALSE,
        run_tests = TRUE,
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


