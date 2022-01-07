## Test environments

* MacOS MBP-M1 (local, R 4.1.2)
* Debian Linux, R-devel, GCC ASAN/UBSAN (R-hub)
* Ubuntu Linux 20.04.1 LTS, R-release, GCC (R-hub)
* Fedora Linux, R-devel, clang, gfortran (R-hub)
* Windows Server 2022, R-devel, 64 bit (R-hub)

## R CMD check results

R CMD check results
0 errors | 0 warnings | 2 notes

R CMD check succeeded


## Downstream dependencies

None


## Misc

First submission to CRAN


## Notes

2 NOTEs are expected during CRAN-submission.

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Ron Keizer <ron@insight-rx.com>'

New submission

Possibly misspelled words in DESCRIPTION:
  Pharmacodynamic (4:25)
  Pharmacokinetic (4:9)
  analytical (19:67)
  pharmacodynamic (18:57)
  pharmacokinetic (18:41)

Maintainer response: These are all correctly spelled words, see:
https://www.merriam-webster.com/dictionary/pharmacodynamic
https://www.merriam-webster.com/dictionary/pharmacokinetic
https://www.merriam-webster.com/dictionary/analytical

* checking dependencies in R code ... NOTE
Namespace in Imports field not imported from: 'BH'
  All declared Imports should be used.

Maintainer response: The BH package provides Boost C++ library headers. The BH library is not used in any of the code in /R or /src, but only in the C++ templates that comes with the package (in /inst/cpp). The C++ templates are used when creating a model using the new_ode_model() function, which is then compiled on-the-fly. If the BH package is removed from the Depends field, it won't be installed and hence models cannot be compiled.

* checking examples ... NOTE
Examples with CPU (user + system) or elapsed time > 5s
      user system elapsed
sim 12.634  0.642  13.279

The example that was previously in \dontrun{} is now generating a Note. This example takes some time to run since it needs to compile a PKPDsim-model. Running of the model itself is very fast, but the example is meant to demonstrate to the user how to compile a model. If the computation time of the example is an issue for CRAN, I can remove the example, or reactivate the \dontrun{} clause again.
