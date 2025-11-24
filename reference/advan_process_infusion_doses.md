# Add column RATEALL to ADVAN-style dataset to handle infusions

Function adapted from code from Abuhelwa, Foster, Upton JPET 2015.
cleaned up and somewhat optimized. Can potentially be optimized more.

## Usage

``` r
advan_process_infusion_doses(data)
```

## Arguments

- data:

  ADVAN-style dataset, e.g. created using `advan_create_data`.

## Value

Data frame containing additional RATEALL column.

## References

Abuhelwa, A. Y., Foster, D. J. R., Upton, R. N. (2015) ADVAN-style
analytical solutions for common pharmacokinetic models. J Pharmacol
Toxicol Methods 73:42-8. DOI: 10.1016/j.vascn.2015.03.004
