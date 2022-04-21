#' More powerful multivariate normal sampling function
#'
#' Besides standard multivariate normal sampling (mvrnorm), allows exponential
#' multivarate normal and quasi-random multivariate normal (using the randtoolbox)
#' all using the same interface.
#'
#' @param n number of samples
#' @param mu mean
#' @param Sigma covariance matrix
#' @param exponential exponential distribution (i.e. multiply mu by exponential of sampled numbers)
#' @param sequence any sequence available in the randtoolbox, e.g. `halton`, or `sobol`
#' @param ... parameters passed to mvrnorm or randtoolbox sequence generator
#' @export
#' @return Multivariate normal samples
mvrnorm2 <- function(n, mu, Sigma, exponential = FALSE, sequence = NULL, ...) {
  if(!is.null(sequence)) {
    func <- switch(
      sequence,
      halton = randtoolbox::halton,
      sobol = randtoolbox::sobol,
      torus = randtoolbox::torus
    )
  }
  if(is.null(dim(Sigma))) { # specified as lower triangle?
    if(inherits(Sigma, "numeric")) {
      Sigma_full <- triangle_to_full(Sigma)
    } else {
      stop("Covariance matrix not specified correctly.")
    }
  } else {
    Sigma_full <- Sigma
  }
  if(!is.null(sequence)) {
    R <- chol(Sigma_full)
    sob <- func(n, dim = ncol(Sigma_full), ...)
    Z <- stats::qnorm(sob) %*% R
  } else {
    Z <- MASS::mvrnorm(n, rep(0, ncol(Sigma_full)), Sigma_full, ...)
  }
  if(exponential) {
    return(t(mu * exp(t(Z))))
  } else {
    return(t(mu + t(Z)))
  }
}
