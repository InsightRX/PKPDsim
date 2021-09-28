#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
DataFrame pk_1cmt_iv_infusion(DataFrame d){

  double k10, t, A1last, Doserate;
  int i;
  DataFrame out = clone(d);

  NumericVector A1 = out["A1"];
  NumericVector DV = out["DV"];
  NumericVector CL = out["CL"];
  NumericVector V  = out["V"];
  NumericVector TIME = out["TIME"];
  NumericVector RATEALL = out["RATEALL"];
  NumericVector AUC = out["AUC"];

  // prepare initial state
  std::vector<int>::iterator it;
  i = 0;
  while(TIME[i] == 0) {
    A1[i] = 0;
    i++;
  }

  // loop over input dataset, calculate microconstants and compartment amounts
  for(i=1; i < A1.size(); i++) {
    k10 = CL[i]/V[i];
    t = TIME[i]-TIME[i-1];
    A1last = A1[i-1];
    Doserate = RATEALL[i];

    A1[i] = Doserate/k10*(1-exp(-t*k10))+A1last*exp(-t*k10);

    DV[i] = A1[i]/V[i];

    if(Doserate > 0) {
      // AUC during infusion is total AUC of dose (A/CL) minus the AUC still to be eliminated (Amount from dose at EOI/CL)
      AUC[i] = AUC[i-1] + (Doserate*t)/CL[i] - (A1[i]-A1last)/CL[i];
    } else {
      AUC[i] = AUC[i-1] + (A1[i-1] - A1[i])/CL[i];
    }

  }

  // Update object
  out["A1"] = A1;
  out["DV"] = DV;
  out["AUC"] = AUC;

  return(out);
}
