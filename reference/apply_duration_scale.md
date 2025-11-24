# Apply infusion duration scale to a regimen

E.g. see Centanni et al. Clin Pharmacokinet 2024. An estimated scaling
factor for the length of the infusion was applied there in a model for
vincristine. This is likely most relevant for very short infusions.

## Usage

``` r
apply_duration_scale(
  regimen,
  duration_scale = NULL,
  parameters = NULL,
  cmt_mapping = NULL
)
```

## Arguments

- regimen:

  PKPDsim regimen

- duration_scale:

  infusion length scale.

- parameters:

  parameter list, required if the duration scale is specified as a
  parameter.

- cmt_mapping:

  map of administration types to compartments, e.g.
  `list("oral" = 1, "infusion" = 2, "bolus" = 2)`.

## Value

Original regimen with infusion lengths scaled by a factor

## Details

Implementation is similar to handling of `lagtime`, i.e. the regimen
that is the input for the simulation function is updated.
