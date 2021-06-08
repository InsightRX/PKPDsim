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
  if ("test1cmtiv" %in% installed.packages()){
    suppressMessages(remove.packages("test1cmtiv"))
  }
  suppressWarnings(
    suppressMessages(
      mod <- model_from_api(
        model = "test_1cmt_iv",
        url = system.file(package = "PKPDsim"),
        to_package = TRUE,
        verbose = FALSE,
        run_tests = TRUE
      )
    )
  )
  # model_from_api will stop if test fails/can't be found
  expect_true("test1cmtiv" %in% installed.packages())
  suppressMessages(require(test1cmtiv))
  mod_from_pkg <- get("model", asNamespace("test1cmtiv"))()
  expect_true("PKPDsim" %in% class(mod_from_pkg))
  if ("test1cmtiv" %in% installed.packages()){
    suppressMessages(remove.packages("test1cmtiv"))
  }
})


