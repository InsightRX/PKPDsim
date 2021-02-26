#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
DataFrame pk_1cmt_iv_bolus(DataFrame d){

  double k10, t, A1last;
  int i;
  DataFrame out = clone(d);

  NumericVector A1 = out["A1"];
  NumericVector DV = out["DV"];
  NumericVector CL = out["CL"];
  NumericVector V  = out["V"];
  NumericVector TIME = out["TIME"];
  NumericVector AMT = out["AMT"];
  NumericVector AUC = out["AUC"];

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
    AUC[i] = AUC[i-1] + (A1[i-1] - A1last*exp(-t*k10))/CL[i];
  }

  // Update object
  out["A1"] = A1;
  out["DV"] = DV;
  out["AUC"] = AUC;

  return(out);
}
