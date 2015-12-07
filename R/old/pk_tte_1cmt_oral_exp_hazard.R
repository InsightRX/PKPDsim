#' ODE system for PK-TTE - 1 compartment oral + hazard
#'
#' @param t time vector
#' @param A initial state vector
#' @param p parameters (list)
#'
#' @export
pk_tte_1cmt_oral_exp_hazard <- function (t, A, p) {
  with(p, {

    ## Parameter translation & calculations
    KEL <-  CL/V

    ## ODE definition:
    return(list(c(
      -KA*A[1] + rate,
      -KEL*A[2] + KA*A[1],
      LAMBDA * (A[2]/V)
    )))
  })
}

## Indicate observation compartment and scaling:
attributes(pk_tte_1cmt_oral_exp_hazard) <- list(obs = list (cmt = 2),
                                                cumhaz = list (cmt = 3))
