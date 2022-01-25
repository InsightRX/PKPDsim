eval_string <- function(s) eval(str2expression(s))

test_that("Transform vector to R code", {
  x <- c("A", "B", "C")
  x_out <- vector_to_R_code(x)
  expect_equal(eval_string(x_out), x)
  expect_true(grepl("^c\\(", x_out))
  expect_true(grepl("\\)$", x_out))
})

test_that("Transform vector to R code: null handling", {
  x <- NULL
  x_null <- vector_to_R_code(x)
  expect_null(eval_string(x_null))
})

test_that("Express bioavailability as parseable R", {
  # assumes appropriate value if bioav specification is missing
  bioav_null <- bioavailability_to_R_code(NULL)
  expect_equal(eval_string(bioav_null), "1")

  # handles single values correctly
  bchar <- "F1i"
  bioav_char <- bioavailability_to_R_code(bchar)
  expect_equal(eval_string(bioav_char), bchar)
  bnum <- 0.9
  bioav_num <- bioavailability_to_R_code(bnum)
  expect_equal(eval_string(bioav_num), as.character(bnum))

  # Handles character vectors correctly
  vec_char <- c("F1i", "1")
  bioav_vec_char <- bioavailability_to_R_code(vec_char)
  expect_equal(eval_string(bioav_vec_char), vec_char)
  expect_true(grepl("^c\\(", bioav_vec_char))
  expect_true(grepl("\\)$", bioav_vec_char))

  # Handles numeric vectors correctly
  vec_num <- c(0.9, 0.7)
  bioav_vec_num <- bioavailability_to_R_code(vec_num)
  expect_equal(eval_string(bioav_vec_num), as.character(vec_num))
  expect_true(grepl("^c\\(", bioav_vec_num))
  expect_true(grepl("\\)$", bioav_vec_num))
})

