#' ODE system for PK - 1 compartment oral administration with sequential zero and 1st order absorption
#'
#' @param t time vector
#' @param A initial state vector
#' @param p parameters (list)
#'
#' @export
#' @examples
#' \dontrun{
#' library(PKPDsim)
#'
#' p <- list(CL = 38.48,
#'           V  = 7.4,
#'           KA = .3,
#'           K0 = 4,
#'           K0_T = 1)
#'
#' r1 <- new_regimen(amt = 100,
#'                   interval = 24,
#'                   n = 10)
#'
#' omega <- cv_to_omega (list(CL = 0.3,
#'                            V = 0.3,
#'                            KA = 0.1,
#'                            K0 = .1,
#'                            K0_T = .3), p)
#'
#' ## sequential k0 and ka
#' sim_ode_shiny(ode = "pk_1cmt_oral_sequential",
#'               par = p,
#'               regimen = new_regimen(amt=30),
#'               omega = omega)
#' }

pk_1cmt_oral_sequential <- function (t, A, p) {
  with(p, {

    ## Parameter translation & calculations
    KEL <-  CL/V

    ## for sequential K0 and KA absorption
    ## the dose times are available in p$dose_times
    idx <- rev((1:length(dose_times))[t>=dose_times])[1]
    tad <- t - dose_times[idx]
    last_dose_amt <- dose_amts[idx]
    K0_T <- last_dose_amt / K0

    ## Note: this is a somewhat crude implementation, as it assumes that all drug
    ##       is absorbed during the drug interval

    ## ODE definition:
    return(list(c(
      K0*(tad <= K0_T) - KA*A[1] * (tad > K0_T),
      -KEL*A[2] + K0*(tad <= K0_T) + KA*A[1] * (tad > K0_T)
    )))
  })
}

## Indicate observation compartment and scaling:
attributes(pk_1cmt_oral_sequential) <- list(obs = list (cmt = 2, scale = "V"))
