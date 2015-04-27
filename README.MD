# Description

A library to perform simulations from a PKPD model, defined using a system of ODEs, model parameter estimates, a dosing regimen, and optionally a between-subject variability matrix. The library also provides a model exploration tool that dynamically generates a Shiny app from the specified model and parameters.

## Rationale

In pharmacometrics, many models are defined as systems of ordinary differential equations (ODEs). Although solving ODEs numerically in R is relatively straightforward using the deSolve library, the implementation of e.g. infusions and complex dosing regimens as well as the simulation of patient populations with multi-level random effects is cumbersome. Therefore, many modelers currently resort to Berkeley Madonna (BM) to perform exploratory simulations instead of performing them in R. BM does provide some excellent interactivity features and is fast, but is: 

- inferior regarding plotting functionality
- cumbersome regarding implementation of dose regimens and multi-level variability 
- not open source/free. 

In essence, the `PKPDsim` tool is an attempt to take the best features of BM, and offer them whithin the R environment so that the modeler can take advantage of e.g. the plotting and visualization tools in R. The library aims to facilitate simulation of any dosing regimen for PKPD mixed-effects models and is implemented as a wrapper around the deSolve library. Inputs for the main function (`sim_ode`) include: a function describing the ODE system, an object describing the regimen, a list of fixed-effects, and a matrix describing the between patient variability. The `PKPDsim` library can dynamically generate Shiny frontends (see figure below) offering features similar to BM. The Shiny app also generates the R code for any plot created in the interface.

![Shiny](images/shiny.png "Shiny example")

Recently, Inria (through the DDMoRe consortium) released a similar library for R (`mlxR`, https://github.com/MarcLavielle/mlxR). The main differences are that `PKPDsim`:

- [+] does not use a proprietary and closed-source simulation engine
- [+/-] does not require/support models written in PharmML
- [+] offers functionality to dynamically generate Shiny apps
- [+] allows specification of PK-PD models by combining separate PK and PD models
- [-] currently does not allow simulation of count data (will follow soon)
- [-] currently does not allow delay differential equations for multidose regimens (will follow)

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
