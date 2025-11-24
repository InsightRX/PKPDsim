# defines C code for TDM before dose conditions

Currently only available for 1-cmt and 2-cmt IV models

## Usage

``` r
define_tdm_init_model(def)
```

## Arguments

- def:

  model definition, named recursive list with at least the objects
  `misc$model_type`, `parameters` and `variables`

## Value

model definition with `state_init` object added describing how to
initializing the compartments.
