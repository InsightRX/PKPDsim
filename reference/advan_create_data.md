# Create ADVAN-style dataset

Create ADVAN-style dataset

## Usage

``` r
advan_create_data(
  regimen,
  parameters,
  cmts = 5,
  t_obs = NULL,
  covariates = NULL,
  covariate_model = NULL
)
```

## Arguments

- regimen:

  PKPDsim regimen

- parameters:

  list of parameters

- cmts:

  number of compartments, minimum is 1. Default is 5, which is enough
  for most linear PK models. It is OK to have more compartments
  available than are actually being used.

- t_obs:

  add observation timepoints to dataset

- covariates:

  covariate list

- covariate_model:

  covariate model equations, written in C

## Value

Data frame of ADVAN-style data
