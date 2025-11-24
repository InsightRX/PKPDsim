# Combines covariates and parameters into a single list, useful for reparametrization of the model.

Combines covariates and parameters into a single list, useful for
reparametrization of the model.

## Usage

``` r
join_cov_and_par(covs, pars)
```

## Arguments

- covs:

  covariates object

- pars:

  model parameters, such as the output of the `parameters()` call frmo a
  model library.

## Value

List containing covariates and parameters
