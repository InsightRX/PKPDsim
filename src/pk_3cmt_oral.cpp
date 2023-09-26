#include <Rcpp.h>
#include <math.h>
using namespace Rcpp;

// [[Rcpp::export]]
DataFrame pk_3cmt_oral(DataFrame d){

  double ka, k20, k23, k32, k24, k42, k30, k40, E2, E3, E4;
  double lambda1, lambda2, lambda3, alpha, beta, gamma, theta;
  double A1last, A2last, A3last, A4last, A2term1, A2term2, A2term3, A3term1, A3term2, A3term3, A4term1, A4term2, A4term3;
  double a, b, c, m, n, q, B, C, I, J, t;
  int i;
  DataFrame out = clone(d);

  NumericVector A1 = out["A1"];
  NumericVector A2 = out["A2"];
  NumericVector A3 = out["A3"];
  NumericVector A4 = out["A4"];
  NumericVector DV = out["DV"];
  NumericVector KA = out["KA"];
  NumericVector CL = out["CL"];
  NumericVector V  = out["V"];
  NumericVector Q  = out["Q"];
  NumericVector V2 = out["V2"];
  NumericVector Q2 = out["Q2"];
  NumericVector V3 = out["V3"];
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
    A4[i] = 0;
    i++;
  }

  // loop over input dataset, calculate microconstants and compartment amounts
  for(i=1; i < A1.size(); i++) {

    k20 = CL[i]/V[i];
    k23 = Q[i]/V[i];
    k32 = k23*V[i]/V2[i];
    k24 = Q2[i]/V[i];
    k42 = k24*V[i]/V3[i];
    ka  = KA[i];
    k30 = 0;
    k40 = 0;
    E2 = k20+k23+k24;
    E3 = k32+k30;
    E4 = k42+k40;

    // calculate hybrid rate constants
    a = E2+E3+E4;
    b = E2*E3+E4*(E2+E3)-k23*k32-k24*k42;
    c = E2*E3*E4-E4*k23*k32-E3*k24*k42;

    m = (3.0*b - pow(a,2.0))/3;
    n = (2.0*pow(a,3.0) - 9.0*a*b + 27.0*c)/27.0;
    q = (pow(n,2.0))/4.0 + (pow(m,3.0))/27.0;

    alpha = sqrt(-1.0*q);
    beta = -1.0*n/2.0;
    gamma = sqrt(pow(beta,2.0)+pow(alpha,2.0));
    theta = atan2(alpha,beta);

    lambda1 = a/3.0 + pow(gamma, 1.0/3.0)*(cos(theta/3.0) + sqrt(3.0)*sin(theta/3.0));
    lambda2 = a/3.0 + pow(gamma, 1.0/3.0)*(cos(theta/3.0) - sqrt(3.0)*sin(theta/3.0));
    lambda3 = a/3.0 -(2.0*pow(gamma, 1.0/3.0)*cos(theta/3.0));

    t = TIME[i]-TIME[i-1];
    A1last = A1[i-1];
    A2last = A2[i-1];
    A3last = A3[i-1];
    A4last = A4[i-1];

    B = A3last*k32+A4last*k42;
    C = E4*A3last*k32+E3*A4last*k42;
    I = A2last*k23*E4-A3last*k24*k42+A4last*k23*k42;
    J = A2last*k24*E3+A3last*k24*k32-A4last*k23*k32;

    A2term1 = A2last*(exp(-t*lambda1)*(E3-lambda1)*(E4-lambda1)/((lambda2-lambda1)*(lambda3-lambda1))+exp(-t*lambda2)*(E3-lambda2)*(E4-lambda2)/((lambda1-lambda2)*(lambda3-lambda2))+exp(-t*lambda3)*(E3-lambda3)*(E4-lambda3)/((lambda1-lambda3)*(lambda2-lambda3)));
    A2term2 = exp(-t*lambda1)*(C-B*lambda1)/((lambda1-lambda2)*(lambda1-lambda3))+exp(-t*lambda2)*(B*lambda2-C)/((lambda1-lambda2)*(lambda2-lambda3))+exp(-t*lambda3)*(B*lambda3-C)/((lambda1-lambda3)*(lambda3-lambda2));
    A2term3 = A1last*ka*(exp(-t*lambda1)*(E3-lambda1)*(E4-lambda1)/((lambda2-lambda1)*(lambda3-lambda1)*(ka-lambda1))+exp(-t*lambda2)*(E3-lambda2)*(E4-lambda2)/((lambda1-lambda2)*(lambda3-lambda2)*(ka-lambda2))+exp(-t*lambda3)*(E3-lambda3)*(E4-lambda3)/((lambda1-lambda3)*(lambda2-lambda3)*(ka-lambda3))+exp(-t*ka)*(E3-ka)*(E4-ka)/((lambda1-ka)*(lambda2-ka)*(lambda3-ka)));
    A2[i] = A2term1+A2term2+A2term3;

    A3term1 = A3last*(exp(-t*lambda1)*(E2-lambda1)*(E4-lambda1)/((lambda2-lambda1)*(lambda3-lambda1))+exp(-t*lambda2)*(E2-lambda2)*(E4-lambda2)/((lambda1-lambda2)*(lambda3-lambda2))+exp(-t*lambda3)*(E2-lambda3)*(E4-lambda3)/((lambda1-lambda3)*(lambda2-lambda3)));
    A3term2 = exp(-t*lambda1)*(I-A2last*k23*lambda1)/((lambda1-lambda2)*(lambda1-lambda3))+exp(-t*lambda2)*(A2last*k23*lambda2-I)/((lambda1-lambda2)*(lambda2-lambda3))+exp(-t*lambda3)*(A2last*k23*lambda3-I)/((lambda1-lambda3)*(lambda3-lambda2));
    A3term3 = A1last*ka*k23*(exp(-t*lambda1)*(E4-lambda1)/((lambda2-lambda1)*(lambda3-lambda1)*(ka-lambda1))+exp(-t*lambda2)*(E4-lambda2)/((lambda1-lambda2)*(lambda3-lambda2)*(ka-lambda2))+exp(-t*lambda3)*(E4-lambda3)/((lambda1-lambda3)*(lambda2-lambda3)*(ka-lambda3))+exp(-t*ka)*(E4-ka)/((lambda1-ka)*(lambda2-ka)*(lambda3-ka)));
    A3[i] = A3term1+A3term2+A3term3;

    A4term1 = A4last*(exp(-t*lambda1)*(E2-lambda1)*(E3-lambda1)/((lambda2-lambda1)*(lambda3-lambda1))+exp(-t*lambda2)*(E2-lambda2)*(E3-lambda2)/((lambda1-lambda2)*(lambda3-lambda2))+exp(-t*lambda3)*(E2-lambda3)*(E3-lambda3)/((lambda1-lambda3)*(lambda2-lambda3)));
    A4term2 = exp(-t*lambda1)*(J-A2last*k24*lambda1)/((lambda1-lambda2)*(lambda1-lambda3))+exp(-t*lambda2)*(A2last*k24*lambda2-J)/((lambda1-lambda2)*(lambda2-lambda3))+exp(-t*lambda3)*(A2last*k24*lambda3-J)/((lambda1-lambda3)*(lambda3-lambda2));
    A4term3 = A1last*ka*k24*(exp(-t*lambda1)*(E3-lambda1)/((lambda2-lambda1)*(lambda3-lambda1)*(ka-lambda1))+exp(-t*lambda2)*(E3-lambda2)/((lambda1-lambda2)*(lambda3-lambda2)*(ka-lambda2))+exp(-t*lambda3)*(E3-lambda3)/((lambda1-lambda3)*(lambda2-lambda3)*(ka-lambda3))+exp(-t*ka)*(E3-ka)/((lambda1-ka)*(lambda2-ka)*(lambda3-ka)));
    A4[i] = A4term1+A4term2+A4term3;

    A1last = A1last*exp(-t*ka);
    A1[i] = A1last + AMT[i]*F1[i];

    DV[i] = A2[i]/V[i];

  }

  // Update object
  out["A1"] = A1;
  out["A2"] = A2;
  out["A3"] = A3;
  out["A4"] = A4;
  out["DV"] = DV;

  return(out);
}
