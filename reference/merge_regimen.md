# Merge two regimens together.

In contrast to `join_regimen`, which joins two consecutive regimens
together, `merge_regimen` merges two or more regimens given at the same
time. This can e.g. be used to define regimens for multi-drug models.

## Usage

``` r
merge_regimen(regimens)
```

## Arguments

- regimens:

  List of PKPDsim regimens created with `new_regimen`.

## Value

Merged regimens
