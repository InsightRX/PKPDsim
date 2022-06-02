test_that("single observation type, scale not supplied", {
  obs1a <- list(
    variable = "CONC"
  )
  expect_equal(
    parse_obs_types(obs1a),
    "      obs.insert(obs.end(), CONC); "
  )
})

test_that("multiple observation type", {
  obs3a <- list(
    variable = c("CONC", "CONCM", "PD")
  )

  res <- PKPDsim:::parse_obs_types(obs3a, initial = FALSE)
  expect_equal(
    res,
    "      if (obs_type[i+1]==1) { obs.insert(obs.end(), CONC); } else if \n         (obs_type[i+1]==2) { obs.insert(obs.end(), CONCM); } else if \n         (obs_type[i+1]==3) { obs.insert(obs.end(), PD); } else \n         { obs.insert(obs.end(), CONC); }"
  )

  res <- PKPDsim:::parse_obs_types(obs3a, initial = TRUE)
  expect_equal(
    res,
    "      if (obs_type[i+0]==1) { obs.insert(obs.end(), CONC); } else if \n         (obs_type[i+0]==2) { obs.insert(obs.end(), CONCM); } else if \n         (obs_type[i+0]==3) { obs.insert(obs.end(), PD); } else \n         { obs.insert(obs.end(), CONC); }"
  )

})

test_that("var and scale provided -> error", {
  expect_error(
    check_obs_input(
      obs = list(variable = c("CONC", "CONCM", "PD"), scale = 1)
    )
  )
})

test_that("cmt and scale mismatch length -> error", {
  expect_error(
    check_obs_input(
      obs = list(cmt = 1, scale = c(1, 0.5))
    )
  )
})

test_that("null cmt or scale get set to 1", {
  obs1 <- list(cmt = 1)
  obs2 <- list(scale = 1)
  expect_equal(
    check_obs_input(obs1),
    list(cmt = 1, scale = 1)
  )
  expect_equal(
    check_obs_input(obs2),
    list(scale = 1, cmt = 1)
  )
})
