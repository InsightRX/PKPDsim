#include <iostream>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <boost/array.hpp>
#include <boost/numeric/odeint.hpp>
#include <Rcpp.h>

using namespace std;
using namespace boost::numeric::odeint;
using namespace Rcpp;

double rate; // infusion rate

