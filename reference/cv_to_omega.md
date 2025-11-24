# Create lower-diagonal omega matrix from CV for parameter estimates

Create lower-diagonal omega matrix from CV for parameter estimates

## Usage

``` r
cv_to_omega(par_cv = NULL, parameters = NULL)
```

## Arguments

- par_cv:

  list of parameter CVs

- parameters:

  list of parameters

## Value

a vector describing the lower triangle of the omega (between-subject
variability) matrix

## See also

[sim_ode](https://insightrx.github.io/PKPDsim/reference/sim_ode.md)
