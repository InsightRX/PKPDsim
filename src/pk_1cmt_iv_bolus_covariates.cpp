#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
DataFrame pk_1cmt_iv_bolus_covariates(DataFrame data, List parameters, StringVector covariates, Function covariate_model){

  double k10, t, A1last;
  int i, j;
  DataFrame out = clone(data);

  NumericVector A1 = out["A1"];
  NumericVector DV = out["DV"];
  NumericVector TIME = out["TIME"];
  NumericVector AMT = out["AMT"];
  NumericVector TMP;
  List data_i = List::create();
  List p_in, p_upd;

  // prepare initial state
  i = 0;
  while(TIME[i] == 0) {
    A1[i] = AMT[i];
    i++;
  }

  // loop over input dataset, calculate microconstants and compartment amounts
  for(i=1; i < A1.size(); i++) {
    for(j = 0; j < covariates.size(); j++) {
      String idx = covariates[j];
      TMP = out[idx];
      data_i[idx] = TMP[i];
    }
    p_in = clone(parameters);
    p_upd = covariate_model(p_in, data_i);
    double CL = p_upd["CL"];
    double V = p_upd["V"];
    k10 = CL/V;
    t = TIME[i] - TIME[i-1];
    A1last = A1[i-1];
    A1[i] = AMT[i] + A1last*exp(-t*k10);
    DV[i] = A1[i]/V;
  }

  // Update object
  out["A1"] = A1;
  out["DV"] = DV;

  return(out);
}
