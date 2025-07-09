test_that("parse_lagtime handles NULL lagtime input", {
  # Mock ODE object without lagtime attribute
  ode <- list()
  parameters <- list(CL = 5, V = 50)
  
  result <- parse_lagtime(NULL, ode, parameters)
  expect_equal(result, 0)
})

test_that("parse_lagtime uses ODE lagtime when user lagtime is NULL", {
  # Mock ODE object with lagtime attribute
  ode <- list()
  attr(ode, "lagtime") <- "TLAG"
  parameters <- list(CL = 5, V = 50, TLAG = 0.6)
  
  result <- parse_lagtime(NULL, ode, parameters)
  expect_equal(result, "TLAG")
})

test_that("parse_lagtime ignores ODE lagtime when user provides lagtime", {
  # Mock ODE object with lagtime attribute
  ode <- list()
  attr(ode, "lagtime") <- "TLAG"
  parameters <- list(CL = 5, V = 50, TLAG = 0.6)
  
  result <- parse_lagtime(1.5, ode, parameters)
  expect_equal(result, 1.5)
})

test_that("parse_lagtime handles character lagtime with valid parameters", {
  ode <- list()
  parameters <- list(CL = 5, V = 50, TLAG = 0.6)
  lagtime <- c("TLAG", "0", "0")
  
  result <- parse_lagtime(lagtime, ode, parameters)
  expect_equal(result, c("TLAG", "0", "0"))
})

test_that("parse_lagtime warns when character lagtime parameters not found", {
  ode <- list()
  parameters <- list(CL = 5, V = 50)
  lagtime <- c("TLAG", "0", "0")
  
  expect_warning(
    parse_lagtime(lagtime, ode, parameters),
    "Lagtime parameter\\(s\\) not found. Please check model and parameters."
  )
})

test_that("parse_lagtime handles numeric lagtime", {
  ode <- list()
  parameters <- list(CL = 5, V = 50)
  lagtime <- 1.5
  
  result <- parse_lagtime(lagtime, ode, parameters)
  expect_equal(result, 1.5)
})

test_that("parse_lagtime ignores NULL and undefined ODE lagtime", {
  # Test NULL lagtime in ODE
  ode1 <- list()
  attr(ode1, "lagtime") <- "NULL"
  parameters <- list(CL = 5, V = 50)
  
  result1 <- parse_lagtime(NULL, ode1, parameters)
  expect_equal(result1, 0)
  
  # Test undefined lagtime in ODE
  ode2 <- list()
  attr(ode2, "lagtime") <- "undefined"
  
  result2 <- parse_lagtime(NULL, ode2, parameters)
  expect_equal(result2, 0)
})

test_that("parse_lagtime handles mixed character and numeric lagtime", {
  ode <- list()
  parameters <- list(CL = 5, V = 50, TLAG1 = 0.6, TLAG2 = 1.2)
  lagtime <- c("TLAG1", "0", "TLAG2")
  
  result <- parse_lagtime(lagtime, ode, parameters)
  expect_equal(result, c("TLAG1", "0", "TLAG2"))
})
