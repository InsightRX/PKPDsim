# Remove n doses (from tail) of PKPDsim regimen

Opposite of shift_regimen()

## Usage

``` r
pop_regimen(regimen, n = 1)
```

## Arguments

- regimen:

  PKPDsim regimen created using
  [`new_regimen()`](https://insightrx.github.io/PKPDsim/reference/new_regimen.md)

- n:

  number of doses to pop from regimen

## Value

Input regiment minus selected number of doses

## See also

shift_regimen
