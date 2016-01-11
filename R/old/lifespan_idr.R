#' DDE system for basic lifespan IDR model
#'
#' @param t time vector
#' @param A initial state vector
#' @param p parameters (list)
#'
#' @export
lifespan_idr <- function(t, A, p) {
  kin <- p$R0/p$TR
  C <- A[1]
  if (t < p$TR) {
    Cdel <- 0
  } else {
    Cdel <- lagvalue(t - p$TR, 1)
  }
  return (list (c(
    -p$kel*A[1],
    kin*(1+p$Smax*C/(p$SC50+C)) - kin*(1+p$Smax*Cdel/(p$SC50+Cdel))
  )))
}
attributes(lifespan_idr) <- list("dde" = TRUE, "obs" = list("cmt" = 2, scale = 1))
