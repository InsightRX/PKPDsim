# Parse observation types to simulation code

Parse observation types to simulation code

## Usage

``` r
parse_obs_types(obs, initial = FALSE)
```

## Arguments

- obs:

  specified observation object including at least a description of which
  variable(s) are associated with a particular compartment, e.g.
  `list(variable="CONC", scale="1")`.

- initial:

  is this for the initial code block in the C++ template that
  initializes the variables and compartments (`TRUE`), or for the second
  code block used for the rest of the dataset?
