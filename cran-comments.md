## Version 1.4.1

The previous version of PKPDsim was removed from the CRAN repository due to an 
issue which CRAN requested to fix. We apologize for the inability to address the
issue within the allotted time. We have addressed the issue now (i.e. removed a 
test with slightly different behavior on macos-arm64 compared to other 
platforms).

We're therefore resubmitting PKPDsim to CRAN, and including these minor fixes:

- removed test for warning in `is_positive_definite()` that is not thrown on macos-arm64
- fixed an edge case where dose-dependent variables or parameters were not correctly initialized
- fixed a bug in the Cpp template for ODE models related to IOV bins that could potentially result in runtime error

Also:
- minor improvements to the README
- grammar and code style fixes in vignettes

## Version 1.4.0

This release fixes an issue introduced with an upgrade in Rcpp (1.0.13), as described here: https://github.com/RcppCore/Rcpp/issues/1311.

Minor other fixes include:
- Fixed the logic regarding a warning about infusion lengths
- Added several example model metadata files, and improve functionality for installing example models
- Ensure that paths with spaces are properly parsed
- Update debugging informational messages
- Make it easier to work with model characteristics
- Make sure infusion length can be used with 'sc' or other non-iv administration types
- Added feature to allow scaling of the infusion duration
- Added feature to calculate AUC pre-ss
- Fix interpolation of time-varying covariates in `pk` blocks.
- Fix regimen_to_nm() conversion function
- Added requirement that metadata has `fixed` definition
- Minor fixes to warnings and messages

## Version 1.3.0

This release addresses the Feb. 16 email from Professor Ripley regarding the use
of Boost headers that are not compliant with recent C++ versions.

- Regarding note "Package in Depends/Imports which should probably only be in LinkingTo: ‘BH’":

The BH package provides Boost C++ library headers. The BH library is not used in any of the code in /R or /src, but only in the C++ templates that comes with the package (in /inst/cpp). The C++ templates are used when creating a model using the new_ode_model() function, which is then compiled on-the-fly. If the BH package is removed from the Depends field, it won't be installed and hence models cannot be compiled and run. BH cannot be moved to the LinkingTo field.
