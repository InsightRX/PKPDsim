
test_that("Correct derivative", {
  expect_equal(
    round(calc_dydP(1:5 + rep(.2, 5), 1:5, .1, FALSE), 3),
    rep(2, 5)
  )
})

test_that("Correct derivative", {
  expect_equal(
    round(calc_dydP(1:5 + rep(.2, 5), 1:5, .1, TRUE), 3),
    c(1.823, 0.953, 0.645, 0.488, 0.392)
  )
})
