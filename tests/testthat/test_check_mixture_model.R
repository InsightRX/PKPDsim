test_that("expected mixture model errors are caught", {
  mixture1 <- list(CL = list(values = c(5, 10), probability = 0.3))
  mixture2 <- list(
    CL = list(values = c(5, 10), probability = 0.3),
    V = list(values = c(10, 20), probability = 0.5)
  )
  mixture_multi <- list(
    CL = list(values = c(5, 6, 7), probability =c(0.3, 0.3, 0.4))
  )
  mixture_oops1 <- list(CL = list(values = c(5, 10), probability = 30))
  mixture_oops2 <- list(CL = list(values = c(5, 10), probability = -0.3))
  expect_error(check_mixture_model(mixture2, c("CL", "V")))
  expect_error(check_mixture_model(mixture1, c("Ke", "V")))
  expect_error(check_mixture_model(mixture_multi, c("CL", "V")))
  expect_error(check_mixture_model(mixture_oops1, c("CL", "V")))
  expect_error(check_mixture_model(mixture_oops2, c("CL", "V")))
})
