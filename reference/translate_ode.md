# Translate a model from/to various PKPD simulators

Currently only supports PKDPsim \<–\> RxODE

## Usage

``` r
translate_ode(code, auto = TRUE, from = NULL, to = NULL, verbose = TRUE)
```

## Arguments

- code:

  character string with ODE code

- auto:

  is auto-detect syntax (`from`)

- from:

  from syntax

- to:

  to syntax

- verbose:

  verbose, `TRUE` or `FALSE`

## Value

Translated PKPDsim or RxODE model
