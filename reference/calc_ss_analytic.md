# Returns the state of a linear PK system at steady state (trough) using analytics equations (so for linear PK systems only).

Basically it performs a PK simulation using analytic equations instead
of ODEs to steady state (n=45 days, increased if needed).

## Usage

``` r
calc_ss_analytic(
  f = "1cmt_oral",
  dose,
  interval,
  t_inf = NULL,
  model,
  parameters,
  covariates = NULL,
  map = NULL,
  n_days = 45,
  n_transit_compartments = 0,
  auc = FALSE
)
```

## Arguments

- f:

  analytic equation to use, must be one of `names(advan_funcs)`

- dose:

  dose

- interval:

  interval

- t_inf:

  infusion time

- model:

  PKPDsim model

- parameters:

  parameters list

- covariates:

  covariates list

- map:

  list for remapping parameters, ex: `list(CL = "CL", V = "V")`

- n_days:

  number of days at which to assume steady state. Default is 45.

- n_transit_compartments:

  number of transit compartments, will insert n compartments between the
  first (dose) compartment and the second (central) compartment.

- auc:

  add (empty) AUC compartment at end of state vector?

## Value

State vector of a linear pharmacokinetic system at steady state

## Details

It can also be used for models with transit compartments, however, the
assumption is made that at the end of the dosing interval the amount in
the transit compartments is negligible (0).
