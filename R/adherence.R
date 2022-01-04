#' Probabilistically model adherence
#'
#' Model the drug adherence using either a binomial probability distribution or
#' a markov chain model based on the probability of staying adherent and of
#' becoming adherent once non-adherent.
#'
#' @param n number of occasions to simulate
#' @param type type of adherence simulation, either "markov" or "binomial"
#' @param p_markov_remain_ad markov probability of staying adherent
#' @param p_markov_become_ad markov probability of going from non-adherent
#'   to adherent state
#' @param p_binom binomial probability of being adherent
#' @return Returns a vector of length `n`
#'   containing values 0 (non-adherent) or 1 (adherent).
#' @export
#' @return Numeric vector of length n
new_adherence <- function(n = 100,
                          type = c("markov", "binomial"),
                          p_markov_remain_ad = 0.75,
                          p_markov_become_ad = 0.75,
                          p_binom = 0.7) {
  type <- match.arg(type)
  switch(
    type,
    "markov" = adherence_markov(n, p_markov_remain_ad, p_markov_become_ad),
    "binomial" = adherence_binomial(n, p_binom)
  )
}

#' Markov adherence model
#'
#' Model adherence as a markov chain model, based on the probability of staying
#' adherent and of becoming adherent once non-adherent. Assumes all patients
#' start adherent.
#'
#' @param n number of occasions
#' @param p11 probability of staying adherent
#' @param p01 probability of going from non-adherent to adherent state
#' @return Returns a vector of length `n`
#'   containing values 0 (non-adherent) or 1 (adherent).
#' @export
#' @return Numeric vector of length n
adherence_markov <- function(n = 100, p11 = 0.9, p01 = 0.7) {
  adh <- 1
  dos <- 1 # all patients adherent for first dose

  if (n < 2) {
    return(adh)
  }

  for (i in seq(2, n)) {
    prob <- ifelse(dos == 1, p11, p01)
    dos <- stats::rbinom(1, 1, prob)
    adh <- c(adh, dos)
  }

  adh
}

#' Binomial adherence
#'
#' Model adherence as a binomial probability at the time of each occasion.
#'
#' @param n number of occasions
#' @param prob binomial probability
#' @return Returns a vector of length `n`
#'   containing values 0 (non-adherent) or 1 (adherent).
#' @export
#' @return Numeric vector of length n
adherence_binomial <- function(n = 100, prob) {
  stats::rbinom(n, 1, prob)
}
