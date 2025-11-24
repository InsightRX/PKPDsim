# R starts counting vector indices at 1, c++ starts at 0, so reduce all state numbers in the Cpp function definition by 1

R starts counting vector indices at 1, c++ starts at 0, so reduce all
state numbers in the Cpp function definition by 1

## Usage

``` r
shift_state_indices(ode_def, n = -1)
```

## Arguments

- ode_def:

  ODE definition

- n:

  add/subtract what number, default = -1
