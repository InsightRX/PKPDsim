# Remove n doses (from start) of PKPDsim regimen

Opposite of pop_regimen()

## Usage

``` r
shift_regimen(regimen, n = 1, reset_time = TRUE)
```

## Arguments

- regimen:

  PKPDsim regimen created using
  [`new_regimen()`](https://insightrx.github.io/PKPDsim/reference/new_regimen.md)

- n:

  number of doses to shift regimen

- reset_time:

  reset the remaining doses to start at t=0?

## Value

Regimen with selected number of doses removed from start

## See also

pop_regimen
