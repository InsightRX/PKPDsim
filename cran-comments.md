## Version 1.3.0

This release addresses the Feb. 16 email from Professor Ripley regarding the use
of Boost headers that are not compliant with recent C++ versions.

- Regarding note "Package in Depends/Imports which should probably only be in LinkingTo: ‘BH’":

The BH package provides Boost C++ library headers. The BH library is not used in any of the code in /R or /src, but only in the C++ templates that comes with the package (in /inst/cpp). The C++ templates are used when creating a model using the new_ode_model() function, which is then compiled on-the-fly. If the BH package is removed from the Depends field, it won't be installed and hence models cannot be compiled and run. BH cannot be moved to the LinkingTo field.
