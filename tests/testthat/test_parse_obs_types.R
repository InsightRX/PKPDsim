test_that("single observation type, scale not supplied", {
  obs1a <- list(
    variable = "CONC"
  )
  expect_equal(
    parse_obs_types(obs1a),
    "      obs.insert(obs.end(), CONC/(1.0)); "
  )
})

test_that("single observation type, scale=variable", {
  obs1b <- list(
    variable = "CONC",
    scale = "V/1000.0"
  )
  expect_equal(
    parse_obs_types(obs1b),
    "      obs.insert(obs.end(), CONC/(V/1000.0)); "
  )
})

test_that("multiple observation type, scale is single", {
  obs3a <- list(
    variable = c("CONC", "CONCM", "PD"),
    scale = 1
  )
  expect_warning(
    res <- parse_obs_types(obs3a),
    "Provided `scale` vector not same length as `variable` vector."
  )
  expect_equal(
    res,
    "      if (obs_type[i+1]==1) { obs.insert(obs.end(), CONC/(1)); } else if \n         (obs_type[i+1]==2) { obs.insert(obs.end(), CONCM/(1)); } else if \n         (obs_type[i+1]==3) { obs.insert(obs.end(), PD/(1)); } else \n         { obs.insert(obs.end(), CONC/(1)); }"
  )
})

test_that("multiple observation type, scale is same-length", {
  obs3b <- list(
    variable = c("CONC", "CONCM", "PD"),
    scale = c(1, 2, 3)
  )
  expect_equal(
    parse_obs_types(obs3b),
    "      if (obs_type[i+1]==1) { obs.insert(obs.end(), CONC/(1)); } else if \n         (obs_type[i+1]==2) { obs.insert(obs.end(), CONCM/(2)); } else if \n         (obs_type[i+1]==3) { obs.insert(obs.end(), PD/(3)); } else \n         { obs.insert(obs.end(), CONC/(1)); }"
  )
})

