# Get expected variance/sd/ci of dependent variable based on PKPDsim model, parameters, and regimen

Get expected variance/sd/ci of dependent variable based on PKPDsim
model, parameters, and regimen

## Usage

``` r
get_var_y(
  model = NULL,
  parameters = list(),
  regimen = list(),
  t_obs = c(1:48),
  obs_comp = NULL,
  obs_variable = NULL,
  omega = c(0.1, 0.05, 0.1),
  omega_full = NULL,
  n_ind = NULL,
  ruv = NULL,
  y = NULL,
  rel_delta = 1e-04,
  method = "delta",
  sequence = NULL,
  auc = FALSE,
  sd = TRUE,
  q = NULL,
  in_parallel = FALSE,
  n_cores = 3,
  return_all = FALSE,
  ...
)
```

## Arguments

- model:

  model, created using
  [`PKPDsim::new_ode_model()`](https://insightrx.github.io/PKPDsim/reference/new_ode_model.md)

- parameters:

  parameters list

- regimen:

  regimen, as created using
  [`PKPDsim::new_regimen()`](https://insightrx.github.io/PKPDsim/reference/new_regimen.md)

- t_obs:

  vector of observation times

- obs_comp:

  observation compartment. If NULL will be "obs" (default)

- obs_variable:

  observation variable. If NULL, will be ignored, otherwise will
  override `obs_comp`.

- omega:

  triangle omega block

- omega_full:

  full omega block

- n_ind:

  number of individuals to simulate with sim method

- ruv:

  residual variability, supplied as a named list, ex:
  `list(prop = 0, add = 0, exp = 0)`

- y:

  vector of observations. If NULL, then a new simulation will be
  performed.

- rel_delta:

  rel_delta

- method:

  method, `delta` or `sim`

- sequence:

  for simulations, if not NULL the pseudo-random sequence to use, e.g.
  "halton" or "sobol". See `mvrnorm2` for more details.

- auc:

  is AUC?

- sd:

  return as standard deviation (`TRUE`) or variance (`FALSE`)

- q:

  return vector of quantiles instead of sd/var. Will return parametric
  quantiles when delta-method is used, non-parametric for
  simulation-based methods.

- in_parallel:

  run simulations in parallel?

- n_cores:

  if run in parallel, on how many cores?

- return_all:

  return object with all relevant information?

- ...:

  passed on to
  [`sim_ode()`](https://insightrx.github.io/PKPDsim/reference/sim_ode.md)

## Value

Vector of standard deviations or variances (or quantiles thereof) for
dependent value variable
