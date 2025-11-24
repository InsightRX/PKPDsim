# Extract sensible default observation times from a specified regimen

Extract sensible default observation times from a specified regimen

## Usage

``` r
get_t_obs_from_regimen(
  regimen = NULL,
  obs_step_size = NULL,
  t_max = NULL,
  covariates = NULL,
  extra_t_obs = NULL,
  t_init = 0
)
```

## Arguments

- regimen:

  regimen created using
  [`new_regimen()`](https://insightrx.github.io/PKPDsim/reference/new_regimen.md)

- obs_step_size:

  step size between observations. Will be auto-calculated if NULL

- t_max:

  max time value

- covariates:

  covariates object, created using `list(new_covariate(), ...)`

- extra_t_obs:

  add timepoints to t_obs at which covariate is changing (`T`/`F`)

- t_init:

  time of initiation of the ODE system. Usually 0.
