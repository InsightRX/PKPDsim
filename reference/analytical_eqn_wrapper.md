# Wrapper for using analytical equations with PKPD regimens

In development. Needs to be optimized significantly to be useful in
production.

## Usage

``` r
analytical_eqn_wrapper(analytical, design = NULL, parameters)
```

## Arguments

- analytical:

  analytical equation, taking parameters `amt`, `parameters`, and `t`,
  and returning a vector of values for `y`

- design:

  design dataset created by `sim_ode`

- parameters:

  list of parameters
