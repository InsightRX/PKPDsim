#' Adherence function
#' @param n number of days to simulate
#' @param type type of adherence simulation, either "markov" or "binomial"
#' @param markov if markov-type, specify a list with markov transition probabilities
#' @param p_binom binomial probability of being adherent
#' @export
new_adherence <- function(n = 100,
                          type = "markov",
                          markov = list(p11 = 0.75, p01 = 0.75),
                          p_binom = 0.7) {
  if(type == "markov") {
    return(adherence_markov(n = n, p11 = markov$p11, p01 = markov$p01))
  } else {
    return(adherence_binomial(n = n, p = p_binom))
  }
}

#' Markov adherence'
#' @param n number of days
#' @param p11 probability of staying adherent
#' @param p01 probability of going from non-adherent to adherent state
#' @export
adherence_markov <- function (n = 100, p11 = 0.9, p01 = 0.7) {
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

#' Binomial adherence'
#' @param n number of days
#' @param p binomial probability
#' @export
adherence_binomial <- function (n = 100, p = 0.5) {
  return(rbinom (n, 1, prob=p))
}
