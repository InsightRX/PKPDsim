#' Adherence function
#' @export
new_adherence <- function(n = 100,
                          markov = list(p11 = 0.75, p01 = 0.75),
                          p_binom = NULL) {
  if(!is.null(markov)) {
      return(adherence_pattern_markov(n = n, p11 = markov$p11, p01 = markov$p01))
  } else {
    return(rbinom (n, 1, prob=p_binom))
  }
}

adherence_pattern_markov <- function (n = 100, p11 = 0.9, p01 = 0.7) {
  adh <- c(1) # all patients adherent for first dose
  dos <- 1
  for (i in 2:n) {
    if (dos == 1) {
      prob <- p11
    } else {
      prob <- p01
    }
    dos <- rbinom(1, 1, prob)
    adh <- c(adh,dos)
  }
  return(adh)
}
