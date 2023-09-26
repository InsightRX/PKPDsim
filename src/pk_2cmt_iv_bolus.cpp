#include <Rcpp.h>
#include <math.h>
using namespace Rcpp;

// [[Rcpp::export]]
DataFrame pk_2cmt_iv_bolus(DataFrame d){

  double k10, k20, k12, k21, E1, E2, lambda1, lambda2, t, A1last, A2last, A1term, A2term;
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
  NumericVector AMT = out["AMT"];
  NumericVector AUC = out["AUC"];

  // prepare initial state
  i = 0;
  while(TIME[i] == 0) {
    A1[i] = AMT[i];
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
    lambda1 = 0.5*(k12 + k21 + k10 + sqrt(pow(k12+k21+k10,2) - 4*k21*k10));
    lambda2 = 0.5*(k12 + k21 + k10 - sqrt(pow(k12+k21+k10,2) - 4*k21*k10));

    t = TIME[i]-TIME[i-1];
    A1last = A1[i-1];
    A2last = A2[i-1];

    A1term = (((A1last*E2+A2last*k21)-A1last*lambda1)*exp(-t*lambda1)-((A1last*E2+A2last*k21)-A1last*lambda2)*exp(-t*lambda2))/(lambda2-lambda1);
    A1[i] = A1term + AMT[i];

    A2term = (((A2last*E1+A1last*k12)-A2last*lambda1)*exp(-t*lambda1)-((A2last*E1+A1last*k12)-A2last*lambda2)*exp(-t*lambda2))/(lambda2-lambda1);
    A2[i] = A2term;
    DV[i] = A1[i]/V[i];

    AUC[i] = AUC[i-1] + (A1[i-1] + A2[i-1] - A1term - A2term)/CL[i];

  }

  // Update object
  out["A1"] = A1;
  out["A2"] = A2;
  out["DV"] = DV;

  return(out);
}
