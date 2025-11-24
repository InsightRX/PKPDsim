# Simulation

The [`sim()`](https://insightrx.github.io/PKPDsim/reference/sim.md)
function will combine the model, the parameters, and the regimen, and
simulate out the ODE system. It will return a `data.frame` in the long
format, i.e. one observation per row, and split by compartment and
individual. The command for
[`sim()`](https://insightrx.github.io/PKPDsim/reference/sim.md) looks
e.g. like this:

``` r
dat <- sim(
  ode = model,              # created using new_ode_model()
  parameters = parameters,  # a named list of parameter values
  regimen = regimen         # created using new_regimen
)
```

Here is a minimal example using real code:

``` r
model <- new_ode_model("pk_1cmt_iv")
parameters <- list(CL = 5, V = 50)
regimen <- new_regimen(
  amt = 100,
  n = 3,
  interval = 12,
  type = "infusion",
  t_inf = 2
)

dat1 <- sim(
  ode = model,
  parameters = parameters,
  regimen = regimen
)
head(dat1)
```

    ##    id t comp        y obs_type
    ## 1   1 0    1  0.00000        1
    ## 3   1 1    1 47.58129        1
    ## 25  1 2    1 90.63462        1
    ## 47  1 3    1 82.00960        1
    ## 65  1 4    1 74.20535        1
    ## 67  1 5    1 67.14378        1

By default, the observation times will include an observation every 1
hour. However, you can specify a vector of observation times to get only
those observations:

``` r
dat2 <- sim(
  ode = model,
  parameters = parameters,
  regimen = regimen,
  t_obs = c(0.5, 2, 4, 8, 12, 16, 24)
)
head(dat2)
```

    ##    id    t comp        y obs_type
    ## 1   1  0.5    1 24.38529        1
    ## 7   1  2.0    1 90.63462        1
    ## 11  1  4.0    1 74.20535        1
    ## 13  1  8.0    1 49.74134        1
    ## 3   1 12.0    1 33.34261        1
    ## 5   1 16.0    1 96.55558        1
