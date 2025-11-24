# Create new ODE model

Create new ODE model

## Usage

``` r
new_ode_model(
  model = NULL,
  code = NULL,
  pk_code = NULL,
  dose_code = NULL,
  file = NULL,
  func = NULL,
  state_init = NULL,
  parameters = NULL,
  reparametrization = NULL,
  mixture = NULL,
  units = NULL,
  size = NULL,
  lagtime = NULL,
  obs = list(cmt = 1, scale = 1),
  dose = list(cmt = 1),
  covariates = NULL,
  declare_variables = NULL,
  iiv = NULL,
  iov = NULL,
  development = NULL,
  omega_matrix = NULL,
  ruv = NULL,
  ltbs = NULL,
  misc = NULL,
  cmt_mapping = NULL,
  int_step_size = NULL,
  default_parameters = NULL,
  fixed = NULL,
  cpp_show_code = FALSE,
  package = NULL,
  test_file = NULL,
  install = TRUE,
  folder = NULL,
  lib_location = NULL,
  verbose = FALSE,
  as_is = FALSE,
  nonmem = NULL,
  comments = NULL,
  version = "0.1.0",
  quiet = "",
  definition = NULL
)
```

## Arguments

- model:

  model name from model library

- code:

  C++ code specifying ODE system

- pk_code:

  C++ code called at any event

- dose_code:

  C++ code called at dose event only

- file:

  file containing C++ code

- func:

  R function to be used with deSolve library

- state_init:

  vector of state init

- parameters:

  list or vector of parameter values

- reparametrization:

  list of parameters with definitions that reparametrize the linear PK
  model to a 1-, 2- o4 3-compartment PK with standardized
  parametrization.

- mixture:

  for mixture models, provide a list of the parameter associated with
  the mixture and it's possible values and probabilities (of the first
  value), e.g. `list(CL = list(value = c(10, 20), probability = 0.3)`.

- units:

  list or vector of parameter units

- size:

  size of state vector for model. Size will be extracted automatically
  from supplied code, use this argument to override.

- lagtime:

  lag time

- obs:

  list with "scale": character string with definition for scale, e.g.
  "V" or "V\*(WT/70)". If NULL, scale defaults to 1., and "cmt" the
  observation compartment

- dose:

  specify default dose compartment, e.g. list(cmt = 1)

- covariates:

  specify covariates, either as a character vector or a list. if
  specified as list, it allows use of timevarying covariates (see
  [`new_covariate()`](https://insightrx.github.io/PKPDsim/reference/new_covariate.md)
  function for more info)

- declare_variables:

  declare variables

- iiv:

  inter-individual variability, can optionally be added to library

- iov:

  inter-occasion variability, can optionally be added to library

- development:

  Information about the model development population, can optionally be
  added to library

- omega_matrix:

  variance-covariance matrix for inter-individual variability, can
  optionally be added to library

- ruv:

  residual variability, can optionally be added to library

- ltbs:

  log-transform both sides. Not used in simulations, only for fitting
  (sets attribute `ltbs`).

- misc:

  a list of miscellaneous model metadata

- cmt_mapping:

  list indicating which administration routes apply to which
  compartments. Example: `list("oral" = 1, "infusion" = 2)`

- int_step_size:

  step size for integrator. Can be pre-specified for model, to override
  default for
  [`sim_ode()`](https://insightrx.github.io/PKPDsim/reference/sim_ode.md)

- default_parameters:

  population or specific patient values, can optionally be added to
  library

- fixed:

  parameters that should not have iiv added.

- cpp_show_code:

  show generated C++ code

- package:

  package name when saving as package

- test_file:

  optional test file to be included with package

- install:

  install package after compilation?

- folder:

  base folder name to create package in

- lib_location:

  install into folder (`--library` argument)

- verbose:

  show more output

- as_is:

  use C-code as-is, don't substitute line-endings or shift indices

- nonmem:

  add NONMEM code as attribute to model object

- comments:

  comments for model

- version:

  number of library

- quiet:

  passed on to `system2` as setting for stderr and stdout; how to output
  cmd line output. Default (`""`) is R console, NULL or FALSE discards.
  TRUE captures the output and saves as a file.

- definition:

  optional, filename for the JSON file the full definition for the
  model. The definition file will be stored as `definition.json` in the
  resulting package.

## Value

If package name is NULL, returns the model object. Otherwise has no
return value.
