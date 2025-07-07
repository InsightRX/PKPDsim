test_that("parse_lagtime works correctly", {
  # Test 1: lagtime provided by user, no ode attribute
  ode1 <- list()
  result1 <- parse_lagtime(lagtime = 5, ode = ode1)
  expect_equal(result1, 5)
  
  # Test 2: no lagtime provided, no ode attribute
  ode2 <- list()
  result2 <- parse_lagtime(lagtime = NULL, ode = ode2)
  expect_equal(result2, c(0))
  
  # Test 3: no lagtime provided, ode has lagtime attribute
  ode3 <- list()
  attr(ode3, "lagtime") <- c(2.5)
  result3 <- parse_lagtime(lagtime = NULL, ode = ode3)
  expect_equal(result3, c(2.5))
  
  # Test 4: lagtime provided by user, ode has lagtime attribute (user takes precedence)
  ode4 <- list()
  attr(ode4, "lagtime") <- c(3)
  result4 <- parse_lagtime(lagtime = 7, ode = ode4)
  expect_equal(result4, 7)
  
  # Test 5: ode lagtime attribute is "undefined"
  ode5 <- list()
  attr(ode5, "lagtime") <- "undefined"
  result5 <- parse_lagtime(lagtime = NULL, ode = ode5)
  expect_equal(result5, c(0))
  
  # Test 6: ode lagtime attribute is "NULL"
  ode6 <- list()
  attr(ode6, "lagtime") <- "NULL"
  result6 <- parse_lagtime(lagtime = NULL, ode = ode6)
  expect_equal(result6, c(0))
  
  # Test 7: ode lagtime attribute is NULL
  ode7 <- list()
  attr(ode7, "lagtime") <- NULL
  result7 <- parse_lagtime(lagtime = NULL, ode = ode7)
  expect_equal(result7, c(0))
  
  # Test 8: vector lagtime inputs
  ode8 <- list()
  result8 <- parse_lagtime(lagtime = c(1, 2, 3), ode = ode8)
  expect_equal(result8, c(1, 2, 3))
  
  # Test 9: vector lagtime from ode attribute
  ode9 <- list()
  attr(ode9, "lagtime") <- c(0.5, 1.5, 2.5)
  result9 <- parse_lagtime(lagtime = NULL, ode = ode9)
  expect_equal(result9, c(0.5, 1.5, 2.5))

  # Test 10: character vector lagtime inputs
  result10 <- parse_lagtime(lagtime = c("TLAG", 0, 0), ode = list())
  expect_equal(result10, c("TLAG", 0, 0))
})
