# ADVAN-style functions to calculate linear PK systems

ADVAN-style functions to calculate linear PK systems

## Usage

``` r
advan(model, cpp = TRUE)
```

## Arguments

- model:

  Standard linear PK model, e.g. `1cmt_iv_bolus`.

- cpp:

  use C++-versions of model (~50x faster than R implementations)

## Value

Model function
