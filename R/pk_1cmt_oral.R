#' ODE system for PK - 1 compartment oral administration
#'
#' @export
pk_1cmt_oral <- function (t, A, p) {
  with(p, {

    ## Parameter translation & calculations
    KEL <-  CL/V

    ## ODE definition:
    return(list(c(
      -KA*A[1] + rate,
      -KEL*A[2] + KA*A[1]
    )))
  })
}

## Indicate observation compartment and scaling:
attributes(pk_1cmt_oral) <- list(obs = list (cmt = 2, scale = "V"))

## C++ version ##############################################################
#' @export
pk_1cmt_oral_cpp <- "
  dAdt[0] = -KA*A[0];
  dAdt[1] = KA*A[0] - (CL/V)*A[1];
"

## Indicate observation compartment and scaling (don't forget "size" argument here!):
attributes(pk_1cmt_oral_cpp) <- list(size = 2, obs = list (cmt = 2, scale = "V"))



