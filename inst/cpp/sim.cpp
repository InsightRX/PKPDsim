runge_kutta4 < state_type > stepr;

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

// [[Rcpp::export]]
NumericMatrix sim_cpp (const NumericVector Ainit, NumericVector times, SEXP par) {
  double step = 1;
  double t_start = times(0);
  double t_end = times(1);
  double dose = 100;
  int n_steps = (int) ceil((t_end-t_start)/step)+1;
  vector<state_type> x_vec;
  vector<double> tim;

  NumericMatrix A_ret (n_steps, n_comp+1);
  std::fill(A_ret.begin(), A_ret.end(), 0);

    Rcpp::List rparam(par);
    std::string method = Rcpp::as<std::string>(rparam["test"]);

    int i = 0;
    state_type A = {} ;
    for(int j = 0; j < n_comp; j++) {
      A[j] = Ainit(j);
    }
    integrate_const (stepr , ode, A, times(0), times(1), step, push_back_solution (x_vec, tim));
    for( size_t i=0; i < n_steps; i++ )
    {
      A_ret(i,0) = tim[i];
       for(int j = 0; j < n_comp; j++) {
         A_ret(i, (j+1)) = x_vec[i][j];
       }
      // cout << tim[i] << '\t' << x_vec[i][0] << '\t' << x_vec[i][1] << '\n';
    }
    return(A_ret);
}

