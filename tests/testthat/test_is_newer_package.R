test_that("Returns true when package version is mismatched", {
  expect_message(
    is_different <- is_newer_package("PKPDsim", "0.01"),
    "Installed version is different"
  )
  expect_true(is_different)
})

test_that("Returns false when package version is identical", {
  current_version <- utils::packageVersion("PKPDsim")
  expect_message(
    is_same <- is_newer_package("PKPDsim", current_version),
    "Installed version is already newest"
  )
  expect_false(is_same)
})

test_that("Returns true when package is not installed", {
  expect_message(
    is_uninstalled <- is_newer_package("TheDefinitivePythonIsBetterThanRPackage", "1.0"),
    "not installed yet"
  )
  expect_true(is_uninstalled)
})
