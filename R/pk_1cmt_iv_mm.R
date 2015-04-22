#' ODE system for PK - 1 compartment IV Michaelis-Menten (non-linear)
#'
#' @export
pk_1cmt_iv_mm <- function (t, A, p) {
  with(p, {

    ## ODE definition:
    CONC <- A[1]/V
    return(list(c(
      -VMAX * CONC / (CONC + KM)
    )))

  })
}
## Indicate observation compartment and scaling:
attributes(pk_1cmt_iv_mm) <- list(obs = list (cmt = 1, scale = "V"))
