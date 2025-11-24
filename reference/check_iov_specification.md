# Checks that IOV was specified appropriately

Inter-occasion variability (IOV) is expected to be supplied as a list
with `cv` and `n_bins` specified. `cv` is expected to be a named list
with IOV for each PK parameter. This function then checks to ensure that
the PK code or ODE code contains an IOV term for each PK parameter
specified.

## Usage

``` r
check_iov_specification(iov, code, pk_code)
```

## Arguments

- iov:

  IOV specifications, provided as a nested named list.

- code:

  C++ ODE code, supplied as a string

- pk_code:

  C++ PK code, supplied as a string
