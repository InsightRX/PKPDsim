## Notes for CRAN reviewer

- The example in sim.R is put within \donttest{} since it often goes over the 5s limit on CRAN. The example is however very useful to users so would strongly prefer to keep it in. The example cannot be made to run faster as it contains a compilation step that takes 4-5 s.

- 2 NOTEs are expected during CRAN-submission.

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Ron Keizer <ron@insight-rx.com>'

New submission

Possibly misspelled words in DESCRIPTION:
  ADVAN (19:9)
  Abuhelwa (19:61)
  Pharmacodynamic (3:45)
  Pharmacokinetic (3:29)
  al (19:73)
  et (19:70)
  pharmacodynamic (17:57)
  pharmacokinetic (17:41)

Maintainer response: These are all correctly spelled words, see:
https://www.merriam-webster.com/dictionary/pharmacodynamic
https://www.merriam-webster.com/dictionary/pharmacokinetic
https://www.merriam-webster.com/dictionary/analytical

ADVAN is an abbreviation that is well known in the field of pharmacometrics. Abuhelwa et al. refers to a paper from which we implemented a simulation method.

* checking dependencies in R code ... NOTE
Namespace in Imports field not imported from: 'BH'
  All declared Imports should be used.

Maintainer response: The BH package provides Boost C++ library headers. The BH library is not used in any of the code in /R or /src, but only in the C++ templates that comes with the package (in /inst/cpp). The C++ templates are used when creating a model using the new_ode_model() function, which is then compiled on-the-fly. If the BH package is removed from the Depends field, it won't be installed and hence models cannot be compiled and run. BH cannot be moved to the LinkingTo field.

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

Initial submission of package to CRAN


