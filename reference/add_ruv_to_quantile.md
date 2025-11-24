# Calculate the increase in a specific quantile for a distribution on y when residual variability is added

Calculate the increase in a specific quantile for a distribution on y
when residual variability is added

## Usage

``` r
add_ruv_to_quantile(y, sd_y, log_scale = FALSE, q = NULL, ruv = list(), ...)
```

## Arguments

- y:

  y with

- sd_y:

  standard deviation of y without residual variability added. Will add
  normally distributed variability (potentially on log-scale).

- log_scale:

  add variability on log scale (FALSE by default, DEPRECATED!).

- q:

  quantile

- ruv:

  list of residual variability (`prop` and `add`)

- ...:

  passed arguments

## Value

Numeric vector of y values with residual variability
