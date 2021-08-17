#include <Rcpp.h>
#include <math.h>
using namespace Rcpp;

// [[Rcpp::export]]
DataFrame pk_2cmt_iv_infusion(DataFrame d){

  double k10, k20, k12, k21, E1, E2, lambda1, lambda2, t;
  double A1last, A2last, A1term1, A1term2, A2term1, A2term2, Doserate;
  int i;
  DataFrame out = clone(d);

  NumericVector A1 = out["A1"];
  NumericVector A2 = out["A2"];
  NumericVector DV = out["DV"];
  NumericVector CL = out["CL"];
  NumericVector V  = out["V"];
  NumericVector Q  = out["Q"];
  NumericVector V2 = out["V2"];
  NumericVector TIME = out["TIME"];
  NumericVector RATEALL = out["RATEALL"];
  NumericVector AUC = out["AUC"];

  // prepare initial state
  std::vector<int>::iterator it;
  i = 0;
  while(TIME[i] == 0) {
    A1[i] = 0;
    A2[i] = 0;
    i++;
  }

  // loop over input dataset, calculate microconstants and compartment amounts
  for(i=1; i < A1.size(); i++) {
    k10 = CL[i]/V[i];
    k12 = Q[i]/V[i];
    k21 = Q[i]/V2[i];
    k20 = 0;
    E1  = k10 + k12;
    E2  = k21 + k20;

    // calculate hybrid rate constants
    lambda1 = 0.5*(k12 + k21 + k10 + sqrt(pow(k12+k21+k10,2.0) - 4.0*k21*k10));
    lambda2 = 0.5*(k12 + k21 + k10 - sqrt(pow(k12+k21+k10,2.0) - 4.0*k21*k10));

    t = TIME[i]-TIME[i-1];
    A1last = A1[i-1];
    A2last = A2[i-1];
    Doserate = RATEALL[i];

    A1term1 = (((A1last*E2+Doserate+A2last*k21)-A1last*lambda1)*exp(-t*lambda1)-((A1last*E2+Doserate+A2last*k21)-A1last*lambda2)*exp(-t*lambda2))/(lambda2-lambda1);
    A1term2 = Doserate*E2*(1/(lambda1*lambda2)+exp(-t*lambda1)/(lambda1*(lambda1-lambda2))-exp(-t*lambda2)/(lambda2*(lambda1-lambda2)));

    A1[i] = A1term1+A1term2;

    A2term1 = (((A2last*E1+A1last*k12)-A2last*lambda1)*exp(-t*lambda1)-((A2last*E1+A1last*k12)-A2last*lambda2)*exp(-t*lambda2))/(lambda2-lambda1);
    A2term2 = Doserate*k12*(1/(lambda1*lambda2)+exp(-t*lambda1)/(lambda1*(lambda1-lambda2))-exp(-t*lambda2)/(lambda2*(lambda1-lambda2)));

    A2[i] = A2term1+A2term2;

    DV[i] = A1[i]/V[i];

    if(Doserate > 0) {
      // AUC during infusion is total AUC of dose (A/CL) minus the AUC still to be eliminated (Amount from dose at EOI/CL)
      AUC[i] = AUC[i-1] + (Doserate*t)/CL[i] - (A1[i]-A1last + A2[i] - A2last)/CL[i];
    } else {
      AUC[i] = AUC[i-1] + (A1[i-1] + A2[i-1]- A1[i] - A2[i])/CL[i];
    }

  }

  // Update object
  out["A1"] = A1;
  out["A2"] = A2;
  out["DV"] = DV;
  out["AUC"] = AUC;

  return(out);
}
