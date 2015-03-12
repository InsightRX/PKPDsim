#' ODE system for PK - 1 compartment IV
#'
#' @export
pk_1cmt_iv <- function (t, A, p) {
  with(p, {

    ## Parameter translation:
    KEL <-  CL/V

        ## ODE definition:
    return(list(c(
      -KEL*A[1] + rate
    )))
  })
}

## Indicate observation compartment and scaling:
attributes(pk_1cmt_iv) <- list(obs = list (cmt = 1, scale = "V"))
