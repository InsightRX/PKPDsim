# Join two dosing regimens

Join two dosing regimens

## Usage

``` r
join_regimen(
  regimen1 = NULL,
  regimen2 = NULL,
  interval = NULL,
  dose_update = NULL,
  t_dose_update = NULL,
  continuous = FALSE
)
```

## Arguments

- regimen1:

  first regimen

- regimen2:

  second regimen

- interval:

  interval between regimen1 and regimen2 (if dose_update not specified)

- dose_update:

  dose number at which to override regimen1 with regimen 2 (if interval
  not specified)

- t_dose_update:

  dose time from which to update regimen

- continuous:

  for joining continuous infusions

## Value

Joined regimen
