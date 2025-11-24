# Load model definition from API, and compile to R library

Load model definition from API, and compile to R library

## Usage

``` r
model_from_api(
  url,
  model = NULL,
  nonmem = NULL,
  verbose = TRUE,
  get_definition = FALSE,
  to_package = FALSE,
  force = FALSE,
  install_all = FALSE,
  ...
)
```

## Arguments

- url:

  URL or file path to JSON representation of model

- model:

  model id (used in messages)

- nonmem:

  URL or file path to NONMEM file

- verbose:

  verbosity (T/F)

- get_definition:

  return only the model definition, do not compile

- to_package:

  compile to package?

- force:

  force install even if same version number of model already installed.

- install_all:

  force install all, even if model inactive

- ...:

  arguments passed to
  [`new_ode_model()`](https://insightrx.github.io/PKPDsim/reference/new_ode_model.md)
  function

## Value

Model object created with
[`new_ode_model()`](https://insightrx.github.io/PKPDsim/reference/new_ode_model.md)
