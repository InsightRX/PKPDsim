runge_kutta_cash_karp54 < state_type > stepr;

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

// [[Rcpp::export]]
List sim_wrapper_cpp (NumericVector A, List design, List par, double step_size) {
  std::vector<double> t;
  std::vector<state_type> y;
  std::vector<double> obs;
  double t_start, t_end;
  std::vector<double> times, doses, rates;
  times = design["t"];
  doses = design["dose"];
  rates = design["rate"];
  int len = times.size();
  int start;
  // insert observation compartment

  // insert covariate definitions

  // insert_state_init
  NumericVector Aupd = clone(A);

  // insert_parameter_definitions
  for(int i = 0; i < (len-1); i++) {
    t_start = times[i];
    t_end = times[(i+1)];
    rate = rates[i];

    // insert covariates for integration period

    // insert scale definition for integration period

    start = 0;
    if(i > 0) {
      start = 1;
    }
    if(strcmp(par["dose_type"], "infusion") != 0) {
      Aupd[0] = Aupd[0] + doses[i];
      if(doses[i] > 0) {
        start = 0;
      }
    }
    ode_out tmp = sim_cpp(Aupd, t_start, t_end, step_size);
    state_type tail = tmp.y.back();
    if(start == 0) {
      t.insert(t.end(), tmp.time.begin(), tmp.time.end());
      y.insert(y.end(), tmp.y.begin(), tmp.y.end());
    } else {
      t.insert(t.end(), boost::next(tmp.time.begin()), tmp.time.end());
      y.insert(y.end(), boost::next(tmp.y.begin()), tmp.y.end());
    }
    for (int k = 0; k < n_comp; k++) {
      Aupd[k] = tail[k];
    }
    for( int k = start; k < tmp.y.size(); k++) {
       obs.insert(obs.end(), tmp.y[k][cmt] / scale);
    }
  }

  List comb;
  comb["time"] = t;
  comb["y"] = y;
  comb["obs"] = obs;
  return(comb);
}

