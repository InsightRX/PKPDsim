# Internal function to parse the raw output from ADVAN-style functions

Internal function to parse the raw output from ADVAN-style functions

## Usage

``` r
advan_parse_output(data, cmts = 1, t_obs, extra_t_obs = TRUE, regimen)
```

## Arguments

- data:

  simulation output data

- cmts:

  number of compartments

- t_obs:

  observation times

- extra_t_obs:

  leave extra added dose times in dataset?

- regimen:

  PKPDsim regimen

## Value

Data frame containing parsed simulation data
