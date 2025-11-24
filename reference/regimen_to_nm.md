# Convert PKPDsim regimen to NONMEM table (doses only)

Convert PKPDsim regimen to NONMEM table (doses only)

## Usage

``` r
regimen_to_nm(
  reg = NULL,
  dose_cmt = 1,
  n_ind = 1,
  t_obs = NULL,
  obs_cmt = 1,
  bioav = NULL
)
```

## Arguments

- reg:

  `PKPDsim` regimen, created using
  [`new_regimen()`](https://insightrx.github.io/PKPDsim/reference/new_regimen.md)
  function

- dose_cmt:

  dosing compartment, if not specified in `reg` object

- n_ind:

  repeat for `n_ind` subjects

- t_obs:

  add observation time(s)

- obs_cmt:

  observation compartment for added observation time(s)

- bioav:

  bioavailability (numeric vector, can not be a parameter)

## Value

Data frame containing doses
