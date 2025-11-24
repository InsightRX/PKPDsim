# Install default literature model

A very lightweight wrapper for `model_from_api` that installs previously
published models packaged within PKPDsim.

## Usage

``` r
install_default_literature_model(model, ...)
```

## Arguments

- model:

  Name of model, e.g., "pk_busulfan_mccune". See
  [`available_default_literature_models()`](https://insightrx.github.io/PKPDsim/reference/available_default_literature_models.md)

- ...:

  arguments passed onto `model_from_api`. For fine-grain control, it is
  better to install models directly from
  [`model_from_api()`](https://insightrx.github.io/PKPDsim/reference/model_from_api.md)
  or
  [`new_ode_model()`](https://insightrx.github.io/PKPDsim/reference/new_ode_model.md).

## Examples

``` r
if (FALSE) { # \dontrun{
install_default_literature_model("pk_busulfan_mccune")
} # }
```
