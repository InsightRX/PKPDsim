## PK 1 compartment IV model
## Author: Ron Keizer
pk_1cmt_iv <- function (t, A, p) {
  p$KEL <-  p$CL/p$V
  return ( list ( c (  dAdt_1 ) ) )
}
attributes(pk_1cmt_iv) <- list(obs = list (cmt = 1, scale = "V"))
