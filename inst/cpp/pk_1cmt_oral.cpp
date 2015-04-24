const double n_comp = 2;
typedef boost::array < double , 2 > state_type;

const double KA = 1.0;
const double CL = 5.0;
const double V = 50;
const double KEL = CL/V;

void ode ( const state_type &A , state_type &dAdt , double t ) {
  dAdt[0] = -KA*A[0];
  dAdt[1] = KA*A[0] - KEL*A[1];
}
