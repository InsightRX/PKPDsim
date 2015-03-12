#' ODE system for PKPD - 1 compartment IV, HIV viral dynamics model (Perelson)
#'
#' @export
pk_1cmt_oral_pd_viral <- function (t, A, p) {
  with(p, {
    ## PK
    K      <- CL / V
    conc   <- A[2] / V
    dAdt_1 <- -KA*A[1]
    dAdt_2 <- -K*A[2] + KA*A[1]
    ## Viral dynamics
    t_bool <- t/24 > tr
    IC50   <- t_bool * Ir + (1-t_bool) * (I0 + ((Ir-I0)/(tr*24))*t )
    gamma  <- conc / (IC50 + conc)
    dAdt_3 <- lambda - d_t*A[3] - (1-gamma)*k_inf*A[3]*A[5]  # T
    dAdt_4 <- (1-gamma) * k_inf*10^(-5) *A[3]*A[5] - delta*A[4] # T*
    dAdt_5 <- N * delta * A[4] - p$c*A[5]  # V
    return ( list ( c (  dAdt_1, dAdt_2, dAdt_3, dAdt_4, dAdt_5 ) ) )
  })
}
attributes(pk_1cmt_oral_pd_viral) <- list(obs = list (cmt = 5, scale = 1))
