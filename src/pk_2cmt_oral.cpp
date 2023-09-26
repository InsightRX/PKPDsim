#include <Rcpp.h>
#include <math.h>
using namespace Rcpp;

// [[Rcpp::export]]
DataFrame pk_2cmt_oral(DataFrame d){

  double ka, k20, k30, k23, k32, E2, E3, lambda1, lambda2, t, A1last, A2last, A3last, A2term1, A2term2, A3term1, A3term2;
  int i;
  DataFrame out = clone(d);

  NumericVector A1 = out["A1"];
  NumericVector A2 = out["A2"];
  NumericVector A3 = out["A3"];
  NumericVector DV = out["DV"];
  NumericVector KA = out["KA"];
  NumericVector CL = out["CL"];
  NumericVector V  = out["V"];
  NumericVector Q  = out["Q"];
  NumericVector V2 = out["V2"];
  NumericVector TIME = out["TIME"];
  NumericVector AMT = out["AMT"];
  NumericVector F1 = out["F1"];
  if(F1==R_NilValue) {
    F1 = rep(1.0, A1.size());
  }

  // prepare initial state
  i = 0;
  while(TIME[i] == 0) {
    A1[i] = AMT[i] * F1[i];
    A2[i] = 0;
    A3[i] = 0;
    i++;
  }

  // loop over input dataset, calculate microconstants and compartment amounts
  for(i=1; i < A1.size(); i++) {
    k20 = CL[i]/V[i];
    k23 = Q[i]/V[i];
    k32 = Q[i]/V2[i];
    ka = KA[i];
    k30 = 0;
    E2 = k20+k23;
    E3 = k32+k30;

    lambda1 = 0.5*((E2+E3)+sqrt(pow(E2+E3, 2.0)-4.0*(E2*E3-k23*k32)));
    lambda2 = 0.5*((E2+E3)-sqrt(pow(E2+E3, 2.0)-4.0*(E2*E3-k23*k32)));

    t = TIME[i]-TIME[i-1];
    A2last = A2[i-1];
    A3last = A3[i-1];
    A1last = A1[i-1];

    A2term1 = (((A2last*E3+A3last*k32)-A2last*lambda1)*exp(-t*lambda1)-((A2last*E3+A3last*k32)-A2last*lambda2)*exp(-t*lambda2))/(lambda2-lambda1);
    A2term2 = A1last*ka*(exp(-t*ka)*(E3-ka)/((lambda1-ka)*(lambda2-ka))+exp(-t*lambda1)*(E3-lambda1)/((lambda2-lambda1)*(ka-lambda1))+exp(-t*lambda2)*(E3-lambda2)/((lambda1-lambda2)*(ka-lambda2)));
    A2[i] = A2term1+A2term2;

    A3term1 = (((A3last*E2+A2last*k23)-A3last*lambda1)*exp(-t*lambda1)-((A3last*E2+A2last*k23)-A3last*lambda2)*exp(-t*lambda2))/(lambda2-lambda1);
    A3term2 = A1last*ka*k23*(exp(-t*ka)/((lambda1-ka)*(lambda2-ka))+exp(-t*lambda1)/((lambda2-lambda1)*(ka-lambda1))+exp(-t*lambda2)/((lambda1-lambda2)*(ka-lambda2)));
    A3[i] = A3term1+A3term2;

    A1last = A1last*exp(-t*ka);
    A1[i] = A1last + AMT[i]*F1[i];

    DV[i] = A2[i]/V[i];

  }

  // Update object
  out["A1"] = A1;
  out["A2"] = A2;
  out["A3"] = A3;
  out["DV"] = DV;

  return(out);
}
