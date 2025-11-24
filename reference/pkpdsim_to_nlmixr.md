# Convert a model generated with PKPDsim to an object for nlmixr

Convert a model generated with PKPDsim to an object for nlmixr

## Usage

``` r
pkpdsim_to_nlmixr(
  model = NULL,
  parameters = NULL,
  omega = NULL,
  res_var = NULL,
  fixed = c(),
  ini_code = NULL,
  model_code = NULL,
  model_par_code = NULL,
  verbose = FALSE,
  ...
)
```

## Arguments

- model:

  PKPDsim model

- parameters:

  list of parameters

- omega:

  vector describing the lower-diagonal of the between-subject
  variability matrix

- res_var:

  residual variability. Expected a list with arguments `prop`, `add`,
  and/or `exp`. NULL by default.

- fixed:

  vector of fixed (not estimated) parameter names

- ini_code:

  manually specify the `ini` block for nlmixr

- model_code:

  manually specify the `model` block for nlmixr

- model_par_code:

  manually specify the parameters section inside the `model` block for
  nlmixr

- verbose:

  verbose, `TRUE` or `FALSE`

- ...:

  passed on

## Value

nlmixr function
