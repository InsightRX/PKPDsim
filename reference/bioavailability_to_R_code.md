# Transforms bioavailability specs into appropriate R code

Specialized wrapper around `vector_to_R_code` that makes reasonable PK
assumptions for when the bioavailability specification is NULL.

## Usage

``` r
bioavailability_to_R_code(bioav)
```

## Arguments

- bioav:

  bioavailability specification, either NULL (assume a value of 1 in all
  compartments), a single value (assume it applies to all compartments),
  or a vector of values.

## Value

character string of length 1
