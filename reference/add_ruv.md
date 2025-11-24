# Add residual variability to the dependent variable

Add residual variability to the dependent variable

## Usage

``` r
add_ruv(x, ruv = list(), obs_type = 1)
```

## Arguments

- x:

  dependent value without residual variability

- ruv:

  list specifying proportional, additive and/or exponential errors
  (`prop`, `add`, `exp`)

- obs_type:

  vector of observation types

## Value

Input vector with residual variability added
