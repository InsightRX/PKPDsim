// euler < state_type > stepr;
runge_kutta4 < state_type > stepr;
// runge_kutta_cash_karp54 < state_type > stepr;

struct push_back_solution
{
  std::vector< state_type >& m_states;
  std::vector< double >& m_times;

  push_back_solution ( std::vector< state_type > &states , std::vector< double > &tim )
    : m_states( states ) , m_times( tim ) { }

  void operator()( const state_type &x , double t )
  {
    m_states.push_back( x );
    m_times.push_back( t );
  }
};

struct ode_out {
  vector<double> time;
  vector<state_type> y;
};

ode_out sim_cpp (const NumericVector Ainit, double t_start, double t_end, double step_size) {
  int n_steps = (int) ceil((t_end-t_start)/step_size);
  vector<state_type> x_vec;
  vector<double> tim;
  NumericMatrix A_ret (n_steps, n_comp+2);
  std::fill(A_ret.begin(), A_ret.end(), 0);
  state_type A = {} ;
  for(int j = 0; j < n_comp; j++) {
    A[j] = Ainit(j);
  }
  ode_out tmp;
  std::vector<double> times( 2 );
  times[0] = t_start;
  times[1] = t_end;
  integrate_times (stepr , ode, A, times, step_size, push_back_solution (x_vec, tim));
  tmp.y    = x_vec;
  tmp.time = tim;
  return(tmp);
}

int first_dose_index(std::vector<double> time, std::vector<int> evid) {
  // find row index for first dose (evid=1, doesn't have to be first event row)
  // returns 0 if no dose given at all
  int index = 0;
  for(int i = 0; i < time.size(); i++) {
    if(evid[i] == 1) {
      index = i;
      break;
    }
  }
  return(index);
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

void set_covariates(int i) {
  // insert covariates for integration period
}

void pk_code (int i, std::vector<double> times, std::vector<double> doses, double prv_dose, std::vector<int> dose_cmt, std::vector<int> dose_type, Rcpp::NumericVector iov_bin) {
  // insert custom pk event code
}

// [[Rcpp::export]]
List sim_wrapper_cpp (NumericVector A, List input_design, List par, NumericVector iov_bins, NumericVector lagtime, double step_size) {
  std::vector<double> t;
  std::vector<state_type> y;
  // insert observation variable definition
  double t_start, t_end;
  std::vector<double> times, doses, dummy, rates;
  std::vector<int> dose_cmt, dose_type, evid, obs_type, y_type;
  // insert variable definitions
  List design = apply_lagtime(input_design, lagtime);

  times = as<std::vector<double> >(design["t"]);
  doses = as<std::vector<double> >(design["dose"]);
  evid = as<std::vector<int> >(design["evid"]);
  dummy = as<std::vector<double> >(design["dum"]);
  rates = as<std::vector<double> >(design["rate"]);
  dose_cmt = as<std::vector<int> >(design["dose_cmt"]);
  dose_type = as<std::vector<int> >(design["type"]);
  obs_type = as<std::vector<int> >(design["obs_type"]);
  int len = times.size();
  int start;
  memset(rate, 0, sizeof(rate));
  // insert observation compartment
  // insert bioavailability definition
  // insert covariate definitions
  // insert_parameter_definitions

  // Initialize parameters, compartments, etc:
  int idx = first_dose_index(times, evid);
  prv_dose = doses[idx];
  pk_code(0, times, doses, prv_dose, dose_cmt, dose_type, iov_bin);

  // call ode() once to pre-calculate any initial variables
  // insert A dAdt state_init
  set_covariates(0);
  t_prv_dose = times[0];

  // Main call to ODE solver, initialize any variables in ode code
  ode(A_dum, dAdt_dum, 0);

  // insert_state_init
  NumericVector Aupd = clone(A);

  for(int i = 0; i < n_comp; i++) { // make sure A and variables in ode block are initialized before start
    A_dum[i] = Aupd[i];
  }
  // call ode() again, make sure variables derived from compartment amounts are initialized
  ode(A_dum, dAdt_dum, 0);

  for(int i = 0; i < (len-1); i++) {
    t_start = times[i];
    t_end = times[(i+1)];

    set_covariates(i);

    if(evid[i] == 1) {
      t_prv_dose = times[i];
      prv_dose = doses[i];
    }
    // insert time-dependent covariates scale
    pk_code(i, times, doses, prv_dose, dose_cmt, dose_type, iov_bin);
    // insert bioav definition
    if(dummy[i] == 1 || (doses[i] > 0 && dose_type[i] == 1)) { // change rate if start of dose, or if end of infusion
      rate[dose_cmt[i]-1] = (rate[dose_cmt[i]-1])*1.0 + rates[i] * bioav[dose_cmt[i]-1];
    }
    // insert scale definition for integration period

    start = 0;
    if(i > 0) {
      start = 1;
    }
    if(evid[i] == 1) {
      if(dose_type[i] == 0) { // bolus
        Aupd[dose_cmt[i]-1] = Aupd[dose_cmt[i]-1] + doses[i] * bioav[dose_cmt[i]-1];
        start = 0;
      }
    }
    ode_out tmp = sim_cpp(Aupd, t_start, t_end, step_size);
    if(start == 0) { // make sure observation variables are stored
      int k = 0;
      // insert scale definition for observation
      // insert saving initial observations to obs object(s)
      // insert copy variables into all variables
    }
    state_type tail = tmp.y.back();
    if(start == 0) {
      t.insert(t.end(), tmp.time.begin(), tmp.time.end());
      y.insert(y.end(), tmp.y.begin(), tmp.y.end());
      y_type.insert(y_type.end(), obs_type[i]);
      y_type.insert(y_type.end(), obs_type[i+1]);
    } else {
      t.insert(t.end(), boost::next(tmp.time.begin()), tmp.time.end());
      y.insert(y.end(), boost::next(tmp.y.begin()), tmp.y.end());
      y_type.insert(y_type.end(), obs_type[i+1]);
    }
    for (int k = 0; k < n_comp; k++) {
      Aupd[k] = tail[k];
    }
    for (int k = 1; k < tmp.y.size(); k++) {
      // insert time-dependent covariates scale
      // insert scale definition for observation
      // insert saving loop observations to obs object(s)
      // insert copy variables into all variables
    }
  }

  List comb;
  comb["time"] = t;
  comb["y"] = y;
  comb["obs_type"] = y_type;
  // insert copy observation object
  // insert copy all variables object
  return(comb);
}
