# PKPDsim <img src="man/figures/hexsticker.png" align="right" width = "200"/>

<!-- badges: start -->
[![R-CMD-check](https://github.com/InsightRX/PKPDsim/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/InsightRX/PKPDsim/actions/workflows/R-CMD-check.yaml)
[![CRAN status](https://www.r-pkg.org/badges/version/PKPDsim)](https://CRAN.R-project.org/package=PKPDsim)
[![R-universe status](https://insightrx.r-universe.dev/badges/PKPDsim)](https://insightrx.r-universe.dev/PKPDsim)
<!-- badges: end -->

PKPDsim is a library for numerical integration of ODE systems, in particular pharmacokinetic-pharmacodynamic (PK-PD) mixed-effects models.

## Installation

The development version of PKPDsim always has the most up-to-date improvements
and bug fixes. We aim to release PKPDsim on CRAN at least once a year,
depending on the number and impact of updates made to the development version.

The [CRAN](https://cran.r-project.org/package=PKPDsim) version of PKPDsim can be installed using:

```r
install.packages("PKPDsim")
```

The development version of PKPDsim can be installed from [GitHub](https://github.com/InsightRX/PKPDsim) or [R-universe](https://insightrx.r-universe.dev/PKPDsim) using:

```r
# Install from GitHub:
devtools::install_github("InsightRX/PKPDsim")

# Install from R-universe:
install.packages("PKPDsim", repos = "https://insightrx.r-universe.dev")
```

A number of models from the literature have been made available for use in
PKPD simulations. To see which models are available for installation, run:

```r
available_default_literature_models()
```

You can install these models as packages using the following commands:

```r
# To install a single model:
install_default_literature_model("pk_busulfan_mccune")

# To install all supplied models:
install_default_literature_model("all")
```

## Contributing

We welcome input from the community:

- If you think you have encountered a bug, please [submit an issue](https://github.com/InsightRX/PKPDsim/issues) 
on the GitHub page. Please include a reproducible example of the unexpected 
behavior.

- Please [open a pull request](https://github.com/InsightRX/PKPDsim/pulls) if
you have a fix or updates that would improve the package. If you're not sure if
your proposed changes are useful or within scope of the package, feel free to
contact one of the authors of this package.

## Disclaimer

The functionality in this R package is provided "as is". While its authors 
adhere to software development best practices, the software may still contain 
unintended errors.

InsightRX Inc. and the authors of this package can not be held liable for any
damages resulting from any use of this software. By the use of this software 
package, the user waives all warranties, expressed or implied, including any 
warranties to the accuracy, quality or suitability of InsightRX for any 
particular purpose, either medical or non-medical.


<div align="right">
© <img src="man/figures/insightrx_logo_color.png" alt="InsightRX logo" width="120" />
</div>
