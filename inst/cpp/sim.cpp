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

void set_covariates(int i) {
  // insert covariates for integration period
}

void pk_code (int i, std::vector<double> times, std::vector<double> doses, double prv_dose, std::vector<int> dose_cmt, std::vector<int> dose_type, Rcpp::NumericVector iov_bin) {
  // insert custom pk event code
}

// [[Rcpp::export]]
List sim_wrapper_cpp (NumericVector A, List design, List par, NumericVector iov_bins, double step_size) {
  std::vector<double> t;
  std::vector<state_type> y;
  // insert observation variable definition
  double t_start, t_end;
  std::vector<double> times, doses, dummy, rates;
  std::vector<int> dose_cmt, dose_type, evid, obs_type, y_type;
  // insert variable definitions
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

  pk_code(0, times, doses, doses[0], dose_cmt, dose_type, iov_bin);
  // call ode() once to pre-calculate any initial variables
  // insert A dAdt state_init
  set_covariates(0);
  prv_dose = doses[0];
  t_prv_dose = times[0];

  // Main call to ODE solver
  ode(A_dum, dAdt_dum, 0);

  // insert_state_init
  NumericVector Aupd = clone(A);

  for(int i = 0; i < (len-1); i++) {
    t_start = times[i];
    t_end = times[(i+1)];

    set_covariates(i);

    if(evid[i] == 1) {
      t_prv_dose = times[i];
      prv_dose = doses[i];
    }
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
    for (int k = start; k < tmp.y.size(); k++) {
      // insert time-dependent covariates scale
      // insert scale definition for observation
      // insert saving observations to obs object(s)
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
