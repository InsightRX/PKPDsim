# Probabilistically model adherence

Model the drug adherence using either a binomial probability
distribution or a markov chain model based on the probability of staying
adherent and of becoming adherent once non-adherent.

## Usage

``` r
new_adherence(
  n = 100,
  type = c("markov", "binomial"),
  p_markov_remain_ad = 0.75,
  p_markov_become_ad = 0.75,
  p_binom = 0.7
)
```

## Arguments

- n:

  number of occasions to simulate

- type:

  type of adherence simulation, either "markov" or "binomial"

- p_markov_remain_ad:

  markov probability of staying adherent

- p_markov_become_ad:

  markov probability of going from non-adherent to adherent state

- p_binom:

  binomial probability of being adherent

## Value

Returns a vector of length `n` containing values 0 (non-adherent) or 1
(adherent).

Numeric vector of length n
