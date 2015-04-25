const double n_comp = 1;
typedef boost::array < double , 1 > state_type;

void ode ( const state_type &A , state_type &dAdt , double t ) {
  double KEL = CL/V;
  dAdt[0] = -KEL*A[0] + rate;
}
