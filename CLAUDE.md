# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

PKPDsim is an R package for simulating pharmacokinetic-pharmacodynamic (PK-PD) mixed-effects models. It uses ODE-based numerical integration (via Rcpp + Boost odeint) and ADVAN-style analytical solutions.

## Commands

```bash
# Load package for interactive development
devtools::load_all()

# Run all tests
devtools::test()

# Run a single test file
testthat::test_file("tests/testthat/test_advan.R")

# Regenerate NAMESPACE and man/ from roxygen2 comments
devtools::document()

# Full R CMD check
devtools::check()

# Build documentation site
pkgdown::build_site()
```

## Architecture

### Core simulation pipeline

```
new_ode_model() / (pre-built ADVAN models)
    ↓
sim()  →  sim_core()  →  C++ ODE solver (Boost odeint RK4)
    ↓
data.frame output (compartment amounts × time)
```

### Key function files

- **`R/new_ode_model.R`** — Defines ODE models: takes user-supplied C++ ODE code as a string, transforms it (index-shifting, auto-declaring variables, injecting infusion rate terms), then compiles it dynamically via `Rcpp::sourceCpp()`. This is the entry point for custom models.
- **`R/sim.R`** — Main user-facing simulation function. Handles input validation, covariate interpolation, IIV/IOV sampling, and dispatch to `sim_core()` or the ADVAN analytical path.
- **`R/sim_core.R`** — Stripped-down simulation wrapper for iterative use (estimation, optimal design); skips validation for performance.
- **`R/advan.R`** — Large (~41KB) file implementing ADVAN1–5 analytical solutions for standard PK models without ODE solving.
- **`R/compile_sim_cpp.R`** — Transforms user ODE strings into compilable C++ and calls `Rcpp::sourceCpp()`.
- **`R/calculate_parameters.R`** — Applies IIV/IOV variability to typical-value parameters.
- **`R/create_event_table.R`** — Builds the design matrix (event table) used by the C++ solver.

### C++ layer

- **`inst/cpp/sim.cpp`** — Template for the dynamically compiled ODE solver using Boost odeint. User ODE code is injected here at compile time.
- **`src/*.cpp`** — Pre-compiled Rcpp functions for standard analytical PK models (1-, 2-, 3-compartment IV/oral).

### Model definition formats

- Custom models: C++ ODE string passed to `new_ode_model(code = "...")`
- Library models: JSON5 files in `inst/models/`, loaded via `read_model_json()`
- API models: fetched at runtime via `model_from_api()`

### Variability modeling

IIV and IOV are applied in `calculate_parameters.R` before simulation. Supported distributions: exponential (`TVPAR * exp(ETA)`) and additive (`TVPAR + ETA`). Mixture models and quasi-random (Halton/Sobol) sampling are supported via `randtoolbox`.

### Testing patterns

Tests live in `tests/testthat/` (51 files). Snapshot tests in `tests/testthat/_snaps/` validate complex numerical outputs. Update snapshots with `testthat::snapshot_review()` when intentional output changes occur.

## CI

GitHub Actions (`.github/workflows/`):
- `pkgdown.yaml`: Documentation website, run for each commit in a PR
- `R-CMD-check.yaml` : R tests, run for each commit in a PR
- `rhub.yaml`: Run tests on Rhub, runs only on request

## Dependencies

Core: `Rcpp` (≥1.0.13), `BH` (Boost headers), `data.table`, `stringr`, `MASS`, `randtoolbox`, `jsonlite`, `parallel`, `magrittr`. R ≥ 4.0.0 required.
