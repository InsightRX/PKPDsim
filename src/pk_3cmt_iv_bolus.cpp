#include <Rcpp.h>
#include <math.h>
using namespace Rcpp;

// [[Rcpp::export]]
DataFrame pk_3cmt_iv_bolus(DataFrame d){

  double k10, k20, k12, k21, k13, k31, k30, E1, E2, E3;
  double lambda1, lambda2, lambda3, alpha, beta, gamma, theta;
  double A1last, A2last, A3last, A1term1, A1term2, A2term1, A2term2, A3term1, A3term2;
  double a, b, c, m, n, q, B, C, I, J, t;
  int i;
  DataFrame out = clone(d);

  NumericVector A1 = out["A1"];
  NumericVector A2 = out["A2"];
  NumericVector A3 = out["A3"];
  NumericVector DV = out["DV"];
  NumericVector CL = out["CL"];
  NumericVector V  = out["V"];
  NumericVector Q  = out["Q"];
  NumericVector V2 = out["V2"];
  NumericVector Q2 = out["Q2"];
  NumericVector V3 = out["V3"];
  NumericVector TIME = out["TIME"];
  NumericVector AMT = out["AMT"];
  NumericVector AUC = out["AUC"];

  // prepare initial state
  i = 0;
  while(TIME[i] == 0) {
    A1[i] = AMT[i];
    A2[i] = 0;
    A3[i] = 0;
    i++;
  }

  // loop over input dataset, calculate microconstants and compartment amounts
  for(i=1; i < A1.size(); i++) {
    k10 = CL[i]/V[i];
    k12 = Q[i]/V[i];
    k21 = k12*V[i]/V2[i];
    k13 = Q2[i]/V[i];
    k31 = k13*V[i]/V3[i];
    k20 = 0;
    k30 = 0;
    E1 = k10+k12+k13;
    E2 = k21+k20;
    E3 = k31+k30;

    a = E1+E2+E3;
    b = E1*E2+E3*(E1+E2)-k12*k21-k13*k31;
    c = E1*E2*E3-E3*k12*k21-E2*k13*k31;

    m = (3.0*b - pow(a,2.0))/3.0;
    n = (2.0*pow(a,3.0) - 9.0*a*b + 27.0*c)/27.0;
    q = (pow(n,2.0))/4.0 + (pow(m,3.0))/27.0;

    alpha = sqrt(-1.0*q);
    beta = -1.0*n/2.0;
    gamma = sqrt(pow(beta,2.0)+pow(alpha,2.0));
    theta = atan2(alpha,beta);

    lambda1 = a/3.0 + pow(gamma,(1.0/3.0))*(cos(theta/3.0) + sqrt(3.0)*sin(theta/3.0));
    lambda2 = a/3.0 + pow(gamma,(1.0/3.0))*(cos(theta/3.0) - sqrt(3.0)*sin(theta/3.0));
    lambda3 = a/3.0 -(2.0*pow(gamma,(1.0/3.0))*cos(theta/3.0));

    t = TIME[i]-TIME[i-1];
    A1last = A1[i-1];
    A2last = A2[i-1];
    A3last = A3[i-1];

    B = A2last*k21+A3last*k31;
    C = E3*A2last*k21+E2*A3last*k31;
    I = A1last*k12*E3-A2last*k13*k31+A3last*k12*k31;
    J = A1last*k13*E2+A2last*k13*k21-A3last*k12*k21;

    A1term1 = A1last*(exp(-t*lambda1)*(E2-lambda1)*(E3-lambda1)/((lambda2-lambda1)*(lambda3-lambda1))+exp(-t*lambda2)*(E2-lambda2)*(E3-lambda2)/((lambda1-lambda2)*(lambda3-lambda2))+exp(-t*lambda3)*(E2-lambda3)*(E3-lambda3)/((lambda1-lambda3)*(lambda2-lambda3)));
    A1term2 = exp(-t*lambda1)*(C-B*lambda1)/((lambda1-lambda2)*(lambda1-lambda3))+exp(-t*lambda2)*(B*lambda2-C)/((lambda1-lambda2)*(lambda2-lambda3))+exp(-t*lambda3)*(B*lambda3-C)/((lambda1-lambda3)*(lambda3-lambda2));
    A1[i] = AMT[i]+(A1term1+A1term2);

    A2term1 = A2last*(exp(-t*lambda1)*(E1-lambda1)*(E3-lambda1)/((lambda2-lambda1)*(lambda3-lambda1))+exp(-t*lambda2)*(E1-lambda2)*(E3-lambda2)/((lambda1-lambda2)*(lambda3-lambda2))+exp(-t*lambda3)*(E1-lambda3)*(E3-lambda3)/((lambda1-lambda3)*(lambda2-lambda3)));
    A2term2 = exp(-t*lambda1)*(I-A1last*k12*lambda1)/((lambda1-lambda2)*(lambda1-lambda3))+exp(-t*lambda2)*(A1last*k12*lambda2-I)/((lambda1-lambda2)*(lambda2-lambda3))+exp(-t*lambda3)*(A1last*k12*lambda3-I)/((lambda1-lambda3)*(lambda3-lambda2));
    A2[i] = A2term1+A2term2;

    A3term1 = A3last*(exp(-t*lambda1)*(E1-lambda1)*(E2-lambda1)/((lambda2-lambda1)*(lambda3-lambda1))+exp(-t*lambda2)*(E1-lambda2)*(E2-lambda2)/((lambda1-lambda2)*(lambda3-lambda2))+exp(-t*lambda3)*(E1-lambda3)*(E2-lambda3)/((lambda1-lambda3)*(lambda2-lambda3)));
    A3term2 = exp(-t*lambda1)*(J-A1last*k13*lambda1)/((lambda1-lambda2)*(lambda1-lambda3))+exp(-t*lambda2)*(A1last*k13*lambda2-J)/((lambda1-lambda2)*(lambda2-lambda3))+exp(-t*lambda3)*(A1last*k13*lambda3-J)/((lambda1-lambda3)*(lambda3-lambda2));
    A3[i] = A3term1+A3term2;

    DV[i] = A1[i]/V[i];

    AUC[i] = AUC[i-1] + (A1[i-1] - (A1term1+A1term2) + A2[i-1] - (A2term1+A2term2) + A3[i-1] - (A3term1+A3term2))/CL[i];

  }

  // Update object
  out["A1"] = A1;
  out["A2"] = A2;
  out["A3"] = A3;
  out["DV"] = DV;
  out["AUC"] = AUC;

  return(out);
}
