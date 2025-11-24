# New covariate

Describe data for a covariate, either fixed or time-variant

## Usage

``` r
new_covariate(
  value = NULL,
  times = NULL,
  implementation = c("interpolate", "locf"),
  unit = NULL,
  interpolation_join_limit = 1,
  remove_negative_times = TRUE,
  round_times = NULL,
  comments = NULL,
  verbose = TRUE
)
```

## Arguments

- value:

  a numeric vector

- times:

  NULL for time-invariant covariate or a numeric vector specifying the
  update times for the covariate

- implementation:

  for time-varying covariates either 'locf' (last observation carried
  forward) or 'interpolate' (default). Non-numeric covariate values are
  assumed to be locf.

- unit:

  specify covariate unit (optional, for documentation purposes only)

- interpolation_join_limit:

  for `interpolate` option, if covariate timepoints are spaced too close
  together, the ODE solver sometimes chokes. This argument sets a lower
  limit on the space between timepoints. It will create average values
  on joint timepoints instead. If undesired set to `NULL` or 0.

- remove_negative_times:

  should times before zero be discarded (with value at time zero
  determined based on `implementation` argument), `TRUE` or `FALSE`.

- round_times:

  round times to specified number of digits. If `NULL`, will not round.

- comments:

  `NULL`, or vector of length equal to `value` specifying comments to
  each observation (optional, for documentation only)

- verbose:

  verbosity

## Value

Object of class `"covariate"`
