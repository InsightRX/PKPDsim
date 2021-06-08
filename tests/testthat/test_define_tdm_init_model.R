test_that("TDM INIT expression for 1-cmt iv", {
  def1 <- list(
    misc = list(model_type = "1cmt_iv"),
    variables = c()
  )
  def1a <- PKPDsim:::define_tdm_init_model(def1)
  expect_true("state_init" %in% names(def1a))
  expect_equal(def1a$parameters$TDM_INIT, 0)
  expect_false(grepl("Vi", def1a$state_init))

  def1$variables <- c("Vi", "CLi")
  def1b <- PKPDsim:::define_tdm_init_model(def1)
  expect_true("state_init" %in% names(def1b))
  expect_equal(def1b$parameters$TDM_INIT, 0)
  expect_true(grepl("Vi", def1b$state_init))
  expect_false(grepl("A\\[1\\]", def1b$state_init))
})

test_that("TDM INIT expression for 2-cmt iv", {
  def2 <- list(
    misc = list(model_type = "2cmt_iv"),
    variables = c()
  )
  def2a <- PKPDsim:::define_tdm_init_model(def2)
  expect_equal(def2a$parameters$TDM_INIT, 0)
  expect_true("state_init" %in% names(def2a))
  expect_false(grepl("Vi", def2a$state_init))
  expect_false(grepl("Qi", def2a$state_init))

  def2$variables <- c("Vi", "CLi")
  def2b <- PKPDsim:::define_tdm_init_model(def2)
  expect_equal(def2b$parameters$TDM_INIT, 0)
  expect_true("state_init" %in% names(def2b))
  expect_true(grepl("Vi", def2b$state_init))
  expect_false(grepl("V2i", def2b$state_init))
  expect_true(grepl("A\\[1\\]", def2b$state_init))

  def2$variables <- c("Vi", "CLi", "V2i")
  def2c <- PKPDsim:::define_tdm_init_model(def2)
  expect_equal(def2c$parameters$TDM_INIT, 0)
  expect_true("state_init" %in% names(def2c))
  expect_true(grepl("Vi", def2c$state_init))
  expect_true(grepl("V2i", def2c$state_init))
  expect_false(grepl("Qi", def2c$state_init))
  expect_true(grepl("A\\[1\\]", def2c$state_init))

  def2$variables <- c("Vi", "CLi", "V2i", "Qi")
  def2d <- PKPDsim:::define_tdm_init_model(def2)
  expect_equal(def2d$parameters$TDM_INIT, 0)
  expect_true("state_init" %in% names(def2d))
  expect_true(grepl("Vi", def2d$state_init))
  expect_true(grepl("V2i", def2d$state_init))
  expect_true(grepl("Qi", def2d$state_init))
  expect_true(grepl("A\\[1\\]", def2d$state_init))
})

test_that("Error if unsupported models", {
  def1 <- list(
    misc = list(model_type = "3cmt_iv_nonlinear"),
    variables = c()
  )
  expect_error(PKPDsim:::define_tdm_init_model(def1))
})

test_that("Error if state initialization already specified", {
  def1 <- list(
    misc = list(model_type = "3cmt_iv_nonlinear"),
    state_init = "A[1] = TDM_INIT * S1",
    variables = c()
  )
  expect_error(PKPDsim:::define_tdm_init_model(def1))
})

