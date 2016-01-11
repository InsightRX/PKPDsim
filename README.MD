[![Build Status](https://travis-ci.org/ronkeizer/PKPDsim.svg?branch=master)](https://travis-ci.org/ronkeizer/PKPDsim)

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

Other R modules with similar aims have recently been released, such as `mlxR` (Inria) and `RxODE` (University of Georgia). While many of the functionality in these packages overlaps with that in `PKPDsim`, some differences are summarized in the table below:

Functionality                         | PKPDsim     | mlxR            | RxODE   |
--------------------------------------------------------------------------------|
Simulation engine                     | C++ odeint  | C++ proprietary | Fortran | 
Open source simulation engine         | +           | -               | +       |
Functionality to generate Shiny apps  | +           | +               | +       |
Simulation of BSV                     | +           | +               | +/-     |
Covariates                            | +           | +               | -       |
Interpolation of timevar. covariates  | +           | -               | -       |
Simulate adherence                    | +           | -               | -       |
Delay differential equations          | -           | +               | -       |
Simulation of count data              | -           | +               | -       |
Available on CRAN                     | -           | -               | +       |


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
