# ADVAN-style equations

Adapted from Abuhelwa et al. JPET 2015

## Usage

``` r
OneCompIVbolus(d)
```

## Arguments

- d:

  data, a NONMEM style data frame for 1 subject with columns for TIME,
  AMT, MDV, DV, CL, V

## Value

Returns a dataframe with populated columns for A1, and DV

## Details

Functions for calculating drug amount in each compartments of the common
pharmacokinetic models (1,2,3 compartment IV bolus, IV infusion, and
first-order absorption models)

Definitions:

- A\*last: is the initial amount at the beginning of each time interval
  (t, t=t2-t1) of a corresponding compartment (i.e. drug amount at the
  end of the last time interval)

- E\* : the sum of Exit (elimination) rate constant of the corresponding
  compartment. IV bolus- 1 compartment

## References

Abuhelwa, A. Y., Foster, D. J. R., Upton, R. N. (2015) ADVAN-style
analytical solutions for common pharmacokinetic models. J Pharmacol
Toxicol Methods 73:42-8. DOI: 10.1016/j.vascn.2015.03.004
