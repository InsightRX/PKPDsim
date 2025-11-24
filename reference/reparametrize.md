# Reparametrize model parameters using a reparametrization defined within the model.

Mostly useful for reparametrizing models into standard parametrizations,
e.g. to NONMEM TRANS or clinPK parametrizations.

## Usage

``` r
reparametrize(model, parameters, covariates)
```

## Arguments

- model:

  PKPDsim model, compiled using `reparametrization` argument or in
  metadata object.

- parameters:

  list of model parameters

- covariates:

  covariates list, specified as PKPDsim covariates

## Value

Reparameterized model parameters
