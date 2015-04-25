## R version #################################################################
#' ODE system for PK - 1 compartment IV
#'
#' @export
pk_1cmt_iv <- function (t, A, p) {
  with(p, {

    ## Parameter translation & calculations
    KEL <-  CL/V

    ## ODE definition:
    return(list(c(
      -KEL*A[1] + rate
    )))
  })
}
## Indicate observation compartment and scaling:
attributes(pk_1cmt_iv) <- list(obs = list (cmt = 1, scale = "V"))


## C++ version ##############################################################
#' @export
pk_1cmt_iv_cpp <- "
  dAdt[1] = -(CL/V)*A[1] + rate ;
"

## Indicate observation compartment and scaling (don't forget "size" argument here!):
attributes(pk_1cmt_iv_cpp) <- list(size = 1, obs = list (cmt = 1, scale = "V"))
