# Compile ODE model to c++ function

Compile ODE model to c++ function

## Usage

``` r
compile_sim_cpp(
  code,
  dose_code,
  pk_code,
  size,
  p,
  cpp_show_code,
  code_init = NULL,
  state_init = NULL,
  declare_variables = NULL,
  variables = NULL,
  covariates = NULL,
  obs = NULL,
  dose = NULL,
  iov = NULL,
  compile = TRUE,
  verbose = FALSE,
  as_is = FALSE
)
```

## Arguments

- code:

  C++ code ODE system

- dose_code:

  C++ code per dose event

- pk_code:

  C++ code per any event (similar to \$PK)

- size:

  size of ODE system

- p:

  parameters (list)

- cpp_show_code:

  show output c++ function?

- code_init:

  code for initialization of state

- state_init:

  state init vector

- declare_variables:

  variable declaration for all required variables (including
  user-specified)

- variables:

  only the user-specified variables

- covariates:

  covariates specification

- obs:

  observation specification

- dose:

  dose specification

- iov:

  iov specification

- compile:

  compile or not?

- verbose:

  show more output

- as_is:

  use C-code as-is, don't substitute line-endings or shift indices

## Value

List containing ODE definition in C++ code and simulation function
