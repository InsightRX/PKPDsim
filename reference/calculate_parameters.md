# Calculate model-specific variables using a dummy call to sim_ode()

This is a convenience function for PKPDsim users, it is not used inside
the
``` sim_ode()`` function in any way. This function is useful for converting from an estimated parameter to actual parameter, e.g. when clearance is specified as  ```CLi
= CL \* (WT/70) \* (1/CR)`it can be used to calculate`CLi\` without
having to write that function a second time in R.

## Usage

``` r
calculate_parameters(
  ode = NULL,
  parameters = NULL,
  covariates = NULL,
  include_parameters = TRUE,
  include_variables = TRUE,
  regimen = NULL,
  t_obs = NULL,
  ...
)
```

## Arguments

- ode:

  PKPDsim model object

- parameters:

  parameter list

- covariates:

  covariate list. Make sure to include covariates at the right time
  point, since only last observed covariate values are used.

- include_parameters:

  boolean, include parameters?

- include_variables:

  boolean, include variables?

- regimen:

  optional, provide a `regimen` object for the computation of the
  effective parameters. This is only relevant for models for which
  parameters depend on the dose or administration type, which is rare.

- t_obs:

  optional, provide timepoint(s) at which to computate effective
  parameters. This is only relevant for models with time-varying
  fixed-effects. If unspecified, will evaluate parameters at `t=0`.

- ...:

  arguments to pass on to simulation function

## Value

List of model-specific variables
