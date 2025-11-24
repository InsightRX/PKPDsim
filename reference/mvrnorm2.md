# More powerful multivariate normal sampling function

Besides standard multivariate normal sampling (mvrnorm), allows
exponential multivariate normal and quasi-random multivariate normal
(using the randtoolbox) all using the same interface.

## Usage

``` r
mvrnorm2(n, mu, Sigma, exponential = FALSE, sequence = NULL, ...)
```

## Arguments

- n:

  number of samples

- mu:

  mean

- Sigma:

  covariance matrix

- exponential:

  exponential distribution (i.e. multiply mu by exponential of sampled
  numbers)

- sequence:

  any sequence available in the randtoolbox, e.g. `halton`, or `sobol`

- ...:

  parameters passed to mvrnorm or randtoolbox sequence generator

## Value

Multivariate normal samples
