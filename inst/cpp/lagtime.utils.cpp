#include "lagtime.utils.h"

NumericVector lagtime_to_numeric(SEXP lagtime, List parameters) {
   NumericVector lagtime_numeric;
  if (TYPEOF(lagtime) == REALSXP) {
    lagtime_numeric = as<NumericVector>(lagtime);
  } else if (TYPEOF(lagtime) == STRSXP) {
    CharacterVector lagtime_char = as<CharacterVector>(lagtime);
    lagtime_numeric = NumericVector(lagtime_char.size());
    for (int i = 0; i < lagtime_char.size(); i++) {
      String param_name = lagtime_char[i];
      if (parameters.containsElementNamed(param_name.get_cstring())) {
        lagtime_numeric[i] = as<double>(parameters[param_name]);
      } else {
        lagtime_numeric[i] = 0.0; // default value
      }
    }
  } else {
    stop("lagtime must be either numeric or character vector");
  }
  return(lagtime_numeric);
}

List apply_lagtime(List design, NumericVector lagtime) {

  List new_design = clone(design);
  std::vector<double> times = as<std::vector<double> >(new_design["t"]);
  std::vector<int> evid = as<std::vector<int> >(new_design["evid"]);
  std::vector<int> cmt = as<std::vector<int> >(new_design["dose_cmt"]);
  
  // Apply lagtime to dose events (evid == 1)
  for(int i = 0; i < times.size(); i++) {
    if(evid[i] == 1) {
      times[i] += lagtime[cmt[i]-1];
    }
  }

  // Create sorted index according to "t"
  std::vector<size_t> indices(times.size());
  std::iota(indices.begin(), indices.end(), 0); // Fill with 0, 1, 2, ...
  std::sort(indices.begin(), indices.end(), [&times](size_t i1, size_t i2) {
    return times[i1] < times[i2];
  });
  
  // Reorder all elements in `t` in new_design
  std::vector<double> sorted_times(times.size());
  for (size_t i = 0; i < indices.size(); i++) {
    sorted_times[i] = times[indices[i]];
  }
  new_design["t"] = sorted_times;
  
  // Sort all other vectors in the design object
  for (const char* key : {"dose", "type", "dum", "dose_cmt", "t_inf", "evid", "bioav", "rate", "obs_type"}) {
    if (new_design.containsElementNamed(key)) {
      SEXP vec = new_design[key];
      if (TYPEOF(vec) == REALSXP) {
        std::vector<double> old_vec = as<std::vector<double> >(vec);
        std::vector<double> new_vec(old_vec.size());
        for (size_t i = 0; i < indices.size(); i++) {
          new_vec[i] = old_vec[indices[i]];
        }
        new_design[key] = new_vec;
      } else if (TYPEOF(vec) == INTSXP) {
        std::vector<int> old_vec = as<std::vector<int> >(vec);
        std::vector<int> new_vec(old_vec.size());
        for (size_t i = 0; i < indices.size(); i++) {
          new_vec[i] = old_vec[indices[i]];
        }
        new_design[key] = new_vec;
      }
    }
  }
  
  return new_design;
}