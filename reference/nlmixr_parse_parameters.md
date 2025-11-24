# Function to parse parameters for a model into a structure used by nlmixr

Function to parse parameters for a model into a structure used by nlmixr

## Usage

``` r
nlmixr_parse_parameters(
  parameters = list(CL = 5, V = 50),
  omega = c(0.1, 0.05, 0.1),
  res_var = list(prop = 0.1, add = 1),
  fixed = c(),
  log_transform = TRUE,
  ...
)
```

## Arguments

- parameters:

  list of parameters

- omega:

  vector describing the lower-diagonal of the between-subject
  variability matrix

- res_var:

  residual variability. Expected a list with arguments `prop`, `add`,
  and/or `exp`. NULL by default.

- fixed:

  vector of fixed parameters

- log_transform:

  log-transform estimated parameters in nlmixr?

- ...:

  passed on

## Value

List of parameters that can be used by nlmixr
