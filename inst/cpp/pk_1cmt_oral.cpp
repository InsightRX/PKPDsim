const double n_comp = 2;
typedef boost::array < double , 2 > state_type;

void ode ( const state_type &A , state_type &dAdt , double t ) {
  double KEL = CL/V;
  dAdt[0] = -KA*A[0];
  dAdt[1] = KA*A[0] - KEL*A[1];
}
