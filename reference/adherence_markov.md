# Markov adherence model

Model adherence as a markov chain model, based on the probability of
staying adherent and of becoming adherent once non-adherent. Assumes all
patients start adherent.

## Usage

``` r
adherence_markov(n = 100, p11 = 0.9, p01 = 0.7)
```

## Arguments

- n:

  number of occasions

- p11:

  probability of staying adherent

- p01:

  probability of going from non-adherent to adherent state

## Value

Returns a vector of length `n` containing values 0 (non-adherent) or 1
(adherent).

Numeric vector of length n
