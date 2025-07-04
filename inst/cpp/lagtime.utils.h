#ifndef LAGTIME_UTILS_H
#define LAGTIME_UTILS_H

#include <Rcpp.h>
using namespace Rcpp;

NumericVector lagtime_to_numeric(SEXP lagtime, List parameters);
List apply_lagtime(List design, NumericVector lagtime);

#endif