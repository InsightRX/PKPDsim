# Only core function of the simulation function, always just returns observations. Mostly useful for estimations / optimal design. Has no checks (for speed)!

Only core function of the simulation function, always just returns
observations. Mostly useful for estimations / optimal design. Has no
checks (for speed)!

## Usage

``` r
sim_core(
  sim_object = NULL,
  ode,
  duplicate_t_obs = FALSE,
  t_init = 0,
  lagtime = c(0)
)
```

## Arguments

- sim_object:

  list with design and simulation parameters

- ode:

  function describing the ODE system

- duplicate_t_obs:

  allow duplicate t_obs in output? E.g. for optimal design calculations
  when t_obs = c(0,1,2,2,3). Default is FALSE.

- t_init:

  initialization time before first dose, default 0.

- lagtime:

  either a value (numeric) or a parameter (character) or NULL.

## Value

data.frame with simulation results
