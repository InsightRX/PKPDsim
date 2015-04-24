const double n_comp = 1;
typedef boost::array < double , 1 > state_type;

const double CL = 5.0;
const double V = 50;
const double KEL = CL/V;

void ode ( const state_type &A , state_type &dAdt , double t ) {
  dAdt[0] = -KEL*A[0];
}
