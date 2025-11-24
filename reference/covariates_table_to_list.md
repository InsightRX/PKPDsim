# Convert covariate table specified as data.frame

Can handle time-varying data too, if `t` or `time` is specified as
column

## Usage

``` r
covariates_table_to_list(covariates_table, covariates_implementation = list())
```

## Arguments

- covariates_table:

  ``` data.frame`` with covariates in columns. Potentially with  ```id`and`t\`
  columns

- covariates_implementation:

  `list` with implementation method per covariate

## Value

List of covariates
