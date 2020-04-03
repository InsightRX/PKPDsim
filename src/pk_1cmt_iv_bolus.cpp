#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
DataFrame pk_1cmt_iv_bolus(DataFrame d){

  double k10, t, A1last;
  int i;

  NumericVector A1 = d["A1"];
  NumericVector DV = d["DV"];
  NumericVector CL = d["CL"];
  NumericVector V  = d["V"];
  NumericVector TIME = d["TIME"];
  NumericVector AMT = d["AMT"];

  // prepare initial state
  std::vector<int>::iterator it;
  i = 0;
  while(TIME[i] == 0) {
    A1[i] = AMT[i];
    i++;
  }

  // loop over input dataset, calculate microconstants and compartment amounts
  for(i=1; i < A1.size(); i++) {
    k10 = CL[i]/V[i];
    t = TIME[i] - TIME[i-1];
    A1last = A1[i-1];
    A1[i] = AMT[i] + A1last*exp(-t*k10);
    DV[i] = A1[i]/V[i];
  }

  // Update object
  d["A1"] = A1;
  d["DV"] = DV;

  return(d);
}
