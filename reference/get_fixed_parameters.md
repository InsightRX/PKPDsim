# Get fixed parameters from model definition.

Get fixed parameters listed in model definition. This function is used
when parsing model specifications before the model has been compiled.
Please see `[get_model_fixed_parameters]` for accessing fixed parameters
from a model that has already been built.

## Usage

``` r
get_fixed_parameters(def)
```

## Arguments

- def:

  Model definition as output by
  [`read_model_json()`](https://insightrx.github.io/PKPDsim/reference/read_model_json.md)
