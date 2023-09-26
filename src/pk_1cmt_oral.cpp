#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
DataFrame pk_1cmt_oral(DataFrame d){

  double k10, ka, t, A1last, A2last;
  int i;
  DataFrame out = clone(d);

  NumericVector A1 = out["A1"];
  NumericVector A2 = out["A2"];
  NumericVector F1 = out["F1"];
  NumericVector DV = out["DV"];
  NumericVector KA = out["KA"];
  NumericVector CL = out["CL"];
  NumericVector V  = out["V"];
  NumericVector TIME = out["TIME"];
  NumericVector AMT = out["AMT"];
  if(F1==R_NilValue) {
    F1 = rep(1.0, A1.size());
  }

  // prepare initial state
  i = 0;
  while(TIME[i] == 0) {
    A1[i] = AMT[i] * F1[i];
    i++;
  }

  // loop over input dataset, calculate microconstants and compartment amounts
  for(i=1; i < A1.size(); i++) {
    k10  = CL[i]/V[i];
    ka   = KA[i];
    t = TIME[i]-TIME[i-1];
    A1last = A1[i-1];
    A2last = A2[i-1];

    A2last = A1last*ka/(ka-k10)*(exp(-t*k10)-exp(-t*ka))+A2last*exp(-t*k10);
    A1last = A1last*exp(-1*t*ka);

    A2[i] = A2last;
    A1[i] = A1last + AMT[i]*F1[i];

    DV[i] = A2[i]/V[i];
  }

  // Update object
  out["A1"] = A1;
  out["DV"] = DV;

  return(out);
}
