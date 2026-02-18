# Convert PKPDsim regimen to NONMEM table (doses only)

Note: when bioavailability is used for insusions: NONMEM behaves
differently from PKPDsim and Monolix, in that rates are not
automatically recomputed for infusions where bioavailabiliy also
applies. In PKPDsim/Monolix, for a bioavailability of 50%, an AMT of 100
mg and rate of 100mg/hr would be recalculated to 50 mg and 50 mg/hour,
respectively, to keep the infusion length the same. This is not the case
in NONMEM. Therefore, the easiest way to work around this is for the
user to compute the rate manually in NONMEM using custom code.
Therefore, we set the `RATE` column to `-1` for such infusions.

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
