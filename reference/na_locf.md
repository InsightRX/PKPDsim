# Fill in NAs with the previous non-missing value

Inspired by zoo::na.locf0

## Usage

``` r
na_locf(object, fromLast = FALSE)
```

## Arguments

- object:

  an object

- fromLast:

  logical. Causes observations to be carried backward rather than
  forward. Default is FALSE.

## Value

Original object with NAs filled in
