test_that("Simulate markov adherence", {
  set.seed(10)
  ans1 <- adherence_markov(150, 0.8, 0.6)
  expect_length(ans1, 150)
  expect_true(all(ans1 %in% c(0, 1)))
  expect_equal(sum(ans1), 123)
  expect_equal(ans1[146:150], c(1, 0, 1, 1, 1))

  set.seed(10)
  ans2 <- new_adherence(n = 150, type = "markov", 0.8, 0.6)
  expect_equal(ans1, ans2)
})

test_that("Simulate binomial adherence", {
  set.seed(10)
  ans3 <- adherence_binomial(150, 0.8)
  expect_length(ans3, 150)
  expect_true(all(ans3 %in% c(0, 1)))
  expect_equal(sum(ans3), 126)
  expect_equal(ans3[146:150], c(0, 1, 1, 1, 0))

  set.seed(10)
  ans4 <- new_adherence(n = 150, type = "binomial", p_binom = 0.8)
  expect_equal(ans3, ans4)
})

test_that("Simulate adherence errors if unreccognized method", {
  expect_error(new_adherence(n = 150, type = "unusual method", prob = 0.8))
})

test_that("Simulate adherence errors if unreccognized method", {
  expect_error(new_adherence(n = 150, type = "proprietary method", prob = 0.8))
})

test_that("Simulate markov adherence of length 1 returns 1", {
  set.seed(10)
  ans1 <- adherence_markov(1, 0.8, 0.6)
  expect_equal(ans1, 1)
})
