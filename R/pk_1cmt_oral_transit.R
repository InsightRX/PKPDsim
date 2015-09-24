#' ODE system for PK - 1 compartment oral administration using transit model
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
#'p_tr <- list(CL = 17.2,
#'             V  = 45.1,
#'             KA = 0.38,
#'             MTT = 2,
#'             N = 20.1,
#'             F = 0) # don't forget to set F=0!
#'
#'omega <- cv_to_omega (list(CL = 0.3,
#'                           V = 0.3,
#'                           KA = 0.1,
#'                           N = 0.3,
#'                           MTT = 0.3), p_tr)
#'
#'sim_ode_shiny(ode = "pk_1cmt_oral_transit",
#'              par = p_tr,
#'              regimen = new_regimen(amt=100, interval=24, n = 1),
#'              omega = omega)
#' }

pk_1cmt_oral_transit <- function (t, A, p) {
  with(p, {

    ## Parameter translation & calculations
    KEL <- CL/V
    KTR = (N+1)/MTT

    ## Transit compartment model as described in Savic et al. JPKPD 2007
    ## Note: this is a somewhat crude implementation, as it assumes that all drug
    ##       is absorbed during the drug interval

    idx <- rev((1:length(dose_times))[t>=dose_times])[1]
    tad <- t - dose_times[idx]
    last_dose_amt <- dose_amts[idx]
    dose_influx <- exp(log(last_dose_amt)+log(KTR)+N*log(KTR*tad)-KTR*tad - (log(2.5066)+(N+0.5)*log(N)-N))

    ## ODE definition:
    return(list(c(
      +dose_influx -KA*A[1],
      -KEL*A[2] + KA*A[1]
    )))
  })
}

## Indicate observation compartment and scaling:
attributes(pk_1cmt_oral_transit) <- list(obs = list (cmt = 2, scale = "V"))
