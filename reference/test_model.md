# Test a model

Test a model

## Usage

``` r
test_model(url, test_file, package, force = FALSE)
```

## Arguments

- url:

  URL or file path to JSON representation of model

- test_file:

  Path to a .R file containing tests to run

- package:

  Package name

- force:

  Run tests even if model is not flagged for building? Defaults to FALSE

## Value

Runs test file for a model but does not return a value
