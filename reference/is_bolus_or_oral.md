# Is regimen type oral or bolus? (i.e., treat t_inf as zero)

Any regimen matching exactly the string 'bolus' or containing the string
'oral' will return `TRUE`, otherwise `FALSE`. Bolus/oral regimens are
generally treated as having an infusion duration of zero.

## Usage

``` r
is_bolus_or_oral(regimen)
```

## Arguments

- regimen:

  object of class "regimen"\`

## Value

Returns a logical vector of length equal to the number of doses in the
regimen
