# Create a regimen from NONMEM data

Create a regimen based on a NONMEM, or NONMEM-like dataset

## Usage

``` r
nm_to_regimen(data, reset_time = TRUE, first_only = FALSE)
```

## Arguments

- data:

  NONMEM-type dataset

- reset_time:

  start time for each simulated patient at 0, irrespective of design in
  dataset

- first_only:

  use only design from first individual in dataset

## Value

Regimen object
