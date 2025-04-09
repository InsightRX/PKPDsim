## Version 1.4.1

The previous version of PKPDsim was removed from the CRAN repository due to an 
issue which CRAN requested to fix. 

We apologize for the inability to address the issue within the allotted time. 
It is unclear to us at this point, however, what the original issue was, 
since the log files have been removed as well. No updates were made to the 
PKPDsim version on CRAN in the months prior to the issue being reported, and 
it seems also no changes were made to CRAN policies in the past 8 months 
(https://github.com/eddelbuettel/crp).

Running R checks on all platform of the current release version resulted in no 
errors or warnings on any platforms 
(https://github.com/InsightRX/PKPDsim/actions/runs/14363056100), and only a 
pre-existing NOTE (see response on version 1.3.0 below).

We're therefore resubmitting PKPDsim to CRAN, while including a few minor fixes:

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
