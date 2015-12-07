#' ODE system for PKPD - 1 compartment IV, HIV viral dynamics model (Perelson)
#'
#' @param t time vector
#' @param A initial state vector
#' @param p parameters (list)
#'
#' @export
pk_1cmt_oral_pd_viral <- function (t, A, p) {
  with(p, {

    ## Parameter translation & calculations
    K      <- CL / V
    conc   <- A[2] / V
    t_bool <- t/24 > tr
    IC50   <- t_bool * Ir + (1-t_bool) * (I0 + ((Ir-I0)/(tr*24))*t )
    gamma  <- conc / (IC50 + conc)

    ## ODE definition:
    return(list(c(
      -KA*A[1] + rate,                                       # A[1]: dose
      -K*A[2] + KA*A[1],                                     # A[2]: central
       lambda - d_t*A[3] - (1-gamma)*k_inf*A[3]*A[5],        # A[3]: T
       (1-gamma) * k_inf*10^(-5) *A[3]*A[5] - delta*A[4],    # A[4]: T*
       N * delta * A[4] - p$c*A[5]                           # A[5]: viral load
    )))
  })
}

## Indicate observation compartment and scaling:
attributes(pk_1cmt_oral_pd_viral) <- list(obs = list (cmt = 5, scale = 1))
