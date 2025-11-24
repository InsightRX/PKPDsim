# Create obs data

Used by [`sim()`](https://insightrx.github.io/PKPDsim/reference/sim.md)
to arrange data from ode() function into the correct format.

## Usage

``` r
create_obs_data(ode_data, obs_attr, id)
```

## Arguments

- ode_data:

  data frame of output from ode() function

- obs_attr:

  "obs" attribute from ode() function

- id:

  ID of the individual

## See also

[`sim()`](https://insightrx.github.io/PKPDsim/reference/sim.md)
