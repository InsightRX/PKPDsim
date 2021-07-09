test_that("Add ruv to sd", {
  sd1 <- add_ruv_to_quantile(10, 3, ruv = list(prop = 0.1, add = 1.5))
  expect_equal(sd1, 3.5)
})

test_that("Add ruv to quantile", {
  q1 <- add_ruv_to_quantile(10, 3, q = 0.6, ruv = list(prop = 0.1, add = 1.5))
  expect_equal(q1, 10.886715)
})


