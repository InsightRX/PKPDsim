[![Build Status](https://travis-ci.org/ronkeizer/PKPDsim.svg?branch=master)](https://travis-ci.org/ronkeizer/PKPDsim)
[![codecov](https://codecov.io/gh/ronkeizer/PKPKDsim/branch/master/graph/badge.svg)](https://codecov.io/gh/ronkeizer/PKPDsim)

# Description

`PKPDsim` is a library for numerical integration of ODE systems, in particular pharmacokinetic-pharmacodynamic (PK-PD) mixed-effects models.

In pharmacometrics, models are often defined as systems of ordinary differential equations (ODEs). Although solving ODEs numerically in R is relatively straightforward using the deSolve library, the implementation of e.g. infusions and complex dosing regimens as well as the incorporation of random effects is cumbersome. A tool like Berkeley Madonna provides excellent interactivity features and is fast[1], but is far inferior to R regarding plotting functionality, cumbersome regarding implementation of dose regimens and multi-level variability, and also not open source/free. This is the rationale for developing the PKPDsim library, i.e. to provide a tool that is:

- easy to use, easy to translate from softwares as NONMEM into R
- flexible and can be used for many different purposes: e.g. teaching, clinical trial simulation, curve fitting, etc.
- fast (through use of compiled code)
- facilitates model / parameter exploration by providing dynamically generated Shiny apps

![Shiny](images/shiny.png "Shiny example")

## Comparison to other tools

Many modelers currently resort to Berkeley Madonna (BM) to perform exploratory simulations instead of performing them in R. BM does provide some excellent interactivity features and is fast, but is:

- much inferior to R regarding plotting functionality
- cumbersome regarding implementation of dose regimens and multi-level variability
- not open source/free.

In essence, the `PKPDsim` tool is an attempt to take the best features of BM, and offer them whithin the R environment and within the context of dosing regimens so that the PKPD modeler can take advantage of e.g. the plotting and visualization tools in R.


Other R modules with similar aims have recently been released. While there is a great deal of overlap between the functionality in these packages, some differences are summarized in the table below (as of Jan 2016):

| Functionality                         | PKPDsim     | mlxR            | RxODE   | mrgsolve    |
|-------------------------------------- | ----------- | --------------- | ------- | ----------- |
| Simulation engine                     | C++ odeint  | C++ proprietary | Fortran | C++         |
| Open source                           | +           | +/- (1)         | +       | +/-  (2)    |
| Available on CRAN                     | -           | +               | +       | -           |
| Creation of Shiny apps                | +           | +               | +       | -           |
| Simulation of BSV                     | +           | +               | +/-     | +           |
| Allows covariates                     | +           | +               | -       | +           |
| Interpolation of timevar. covariates  | +           | -               | -       | -           |
| Use NONMEM datasets                   | + / -       | -               | -       | +           |
| Simulate adherence                    | +           | -               | -       | -           |
| Delay differential equations          | -           | +               | -       | -           |
| Simulation of count data              | -           | +               | -       | -           |

*The above table was created to the best of my knowledge and the documentation of the various packages. Please let me know if you find any inaccuracies.*

*1: the underlying simulation engine is proprietary software (although free for academia)*

*2: released under GPL, but source code only provided on request*

## Future plans
- release to CRAN
- add more example models / tutorials

## Installation

```
library(devtools)
install_github("ronkeizer/PKPDsim")
library(PKPDsim)
```

## How to use

See [vignette](http://ronkeizer.github.io/PKPDsim/)

## License

[MIT license](http://opensource.org/licenses/MIT)
