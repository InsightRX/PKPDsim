# AGENTS.md

This file provides guidance to AI coding agents when working with code in this
repository. It is harness-agnostic: any agent that reads repo-level instructions
should use it.

## Overview

PKPDsim is an R package for simulating pharmacokinetic-pharmacodynamic (PK-PD)
mixed-effects models. It uses ODE-based numerical integration (via Rcpp + Boost
odeint) and ADVAN-style analytical solutions.

The package is on CRAN, so changes must keep `R CMD check --as-cran` clean.

## Commands

```r
# Load package for interactive development
devtools::load_all()

# Run all tests
devtools::test()

# Run a single test file
testthat::test_file("tests/testthat/test_advan.R")

# Run a subset of test files (preferred: this also runs tests/testthat/setup.R,
# which many test files depend on for their shared, pre-compiled models)
devtools::test(filter = "advan")

# Regenerate NAMESPACE and man/ from roxygen2 comments
devtools::document()

# Full R CMD check
devtools::check()

# Build documentation site
pkgdown::build_site()
```

Notes:

- Run `devtools::document()` after touching any roxygen2 block; `NAMESPACE` and
  `man/*.Rd` are generated, never edit them by hand.
- Tests and examples that compile models are slow. Several are gated behind
  `NOT_CRAN=true` (see `tests/testthat/setup.R`); set that env var locally to
  exercise the full suite.
- C++ in `src/` is compiled at package install time. After editing `src/*.cpp`,
  re-run `devtools::load_all()` (which recompiles) rather than relying on a
  cached build. If a `src/RcppExports.cpp` signature changes, run
  `Rcpp::compileAttributes()`.

## Architecture

### Core simulation pipeline

```
new_ode_model() / (pre-built ADVAN models)
    ↓
sim()  →  compiled C++ ODE solver (Boost odeint RK4)
    ↓
data.frame output (compartment amounts × time)
```

`sim()` and `sim_core()` are siblings, not a chain: `sim()` never calls
`sim_core()`. Both end up calling the compiled `ode` object directly.

`sim()` branches: if `analytical` is supplied it skips ODE solving entirely and
routes through `advan()` / `advan_create_data()` / `advan_parse_output()`
(`R/sim.R`, around the "Use analytical equations (ADVAN)" block). Otherwise it
builds an event table and calls the dynamically compiled ODE solver.

### Key function files

- **`R/new_ode_model.R`** — Defines ODE models: takes user-supplied C++ ODE code as a string, transforms it (index-shifting, auto-declaring variables, injecting infusion rate terms), then compiles it dynamically via `Rcpp::sourceCpp()`. This is the entry point for custom models.
- **`R/sim.R`** — Main user-facing simulation function. Handles input validation, covariate interpolation, IIV sampling and application, event-table construction, and then either calls the compiled `ode` object or routes to the ADVAN analytical path.
- **`R/sim_core.R`** — Separate fast entry point for iterative use (estimation, optimal design). It consumes a precalculated design — e.g. the object from `sim(..., return_design = TRUE)` — and calls the compiled `ode` with no validation.
- **`R/advan.R`** — Large (~41KB) file implementing ADVAN1–5 analytical solutions for standard PK models without ODE solving.
- **`R/compile_sim_cpp.R`** — Transforms user ODE strings into compilable C++ and calls `Rcpp::sourceCpp()`.
- **`R/shift_state_indices.R`** — Rewrites compartment indices in ODE code (R is 1-based, C++ is 0-based) before compilation.
- **`R/translate_ode.R`** — Translates ODE code between PKPDsim and RxODE syntax.
- **`R/calculate_parameters.R`** — Convenience wrapper around `sim_ode()` that returns model-specific / effective parameters and variables (e.g. `CLi` from `CL` and covariates). Not used by `sim()` itself.
- **`R/create_event_table.R`** — Builds the design matrix (event table) used by the C++ solver.
- **`R/model_library.R`** — Named library of standard structural models (`pk_1cmt_iv`, `pk_2cmt_iv_auc`, `pk_1cmt_iv_mm`, `pk_*_oral`, …), each a list of ODE code plus `obs`/`dose` defaults. This is what `new_ode_model("pk_1cmt_iv")` resolves against.
- **`R/new_regimen.R`**, **`R/new_covariate.R`** — Constructors for the two other main user-facing inputs to `sim()`; regimen joining/shifting helpers live in `R/join_regimen.R`, `R/shift_regimen.R`, `R/merge_regimen.R`.

### C++ layer

- **`inst/cpp/sim.cpp`**, **`inst/cpp/sim_header.cpp`** — Template for the dynamically compiled ODE solver using Boost odeint. User ODE code is injected here at compile time.
- **`src/*.cpp`** — Pre-compiled Rcpp functions for standard analytical PK models (1-, 2-, 3-compartment IV/oral).
- **`src/Makevars`** — Sets `PKG_CPPFLAGS = -D_HAS_AUTO_PTR_ETC=0` (needed for Boost with newer C++ standards).

Two distinct compilation paths exist and should not be confused: `src/` is
compiled once when the package is installed, while user ODE models are compiled
at *runtime* into a temporary shared object (or an installed model package).

### Model definition formats

- Custom models: C++ ODE string passed to `new_ode_model(code = "...")`
- Library models: named entries in `R/model_library.R`, e.g. `new_ode_model("pk_1cmt_iv")`
- Literature models: JSON5 files in `inst/models/`, loaded via `read_model_json()` and installable as standalone packages via `install_default_literature_model()`
- API models: fetched at runtime via `model_from_api()`

When `new_ode_model(package = "...")` is used, the model is emitted as a real R
package scaffolded from **`inst/template/`** (its `R/` files carry parameters,
IIV, IOV, RUV, fixed parameters and the compiled model object). Changes to model
metadata usually need a matching change in that template.

### ODE code conventions

User-facing ODE code may index compartments from either `A[0]` or `A[1]`.
`shift_state_indices()` subtracts 1 from every index, but returns the code
untouched if that would produce a negative index — so already-0-based code is
left alone. `new_ode_model(as_is = TRUE)` disables this rewriting (and
line-ending substitution) entirely. Variables that are not
parameters or covariates must be listed in `declare_variables`, otherwise the
generated C++ will not compile. `pk_code` runs once at initialization and then
at each event, before that interval is integrated — not before every derivative
evaluation, since RK4 evaluates the ODE several times within an interval, so do
not put per-derivative logic there. `state_init` initializes compartment
amounts.

### Variability modeling

IIV is sampled and applied in `R/sim.R`: `mvrnorm2()` draws the ETAs, and the
`omega_type` block applies them per parameter. Accepted `omega_type` values are
`exponential` (`TVPAR * exp(ETA)`) and `normal` (`TVPAR + ETA`).

IOV is different: `R/new_ode_model.R` generates the `kappa_*` bin logic into the
model C++ at compile time, and the compiled solver picks the right bin at
runtime from the `iov_bins` passed through `sim()`.

Mixture models and quasi-random (Halton/Sobol) sampling are supported via
`randtoolbox`.

### Interoperability

`R/pkpdsim_to_nlmixr.R`, `R/nlmixr_parse_parameters.R`, `R/regimen_to_nm.R` and
`R/nm_to_regimen.R` convert models and regimens to/from nlmixr2 and NONMEM.

## Testing patterns

Tests live in `tests/testthat/` (testthat edition 3).

- **`tests/testthat/setup.R` compiles a set of shared models once per test
  session** (`mod_1cmt_iv`, `pk_iov`, `mod_mixture`, …). Reuse those objects
  instead of calling `new_ode_model()` inside a test — compilation dominates
  runtime. Add new shared fixtures there when several files need them.
- Expensive fixtures in `setup.R` are wrapped in
  `if (identical(Sys.getenv("NOT_CRAN"), "true"))`; tests that use them must be
  skipped under the same condition (`skip_on_cran()`).
- The test suite does not hit the network: `test_model_from_api.R` points
  `model_from_api()` at local fixtures in `tests/testthat/sample_json/`. Keep it
  that way when adding tests for API-backed models.
- The one test that actually installs a model as a package
  (`test_model_from_api.R`, "Can install a package from json") writes to an
  explicit temp lib dir and is gated with `skip_on_cran()`. Follow that pattern
  for any new installing test; tests that only exercise dispatch (e.g.
  `test_install_default_literature_model.R`) mock `model_from_api()` instead and
  need neither.

## CI

GitHub Actions (`.github/workflows/`):
- `R-CMD-check.yaml`: `R CMD check --as-cran` on ubuntu/windows/macOS (R release), run for each push to `master` and each commit in a PR. Fails only on errors, not warnings/notes.
- `pkgdown.yaml`: Documentation website, run for each commit in a PR
- `rhub.yaml`: Run tests on R-hub, runs only on request

## Dependencies

Core: `Rcpp` (≥1.0.13), `BH` (Boost headers), `data.table`, `stringr`, `MASS`, `randtoolbox`, `jsonlite`, `stats`, `parallel`, `magrittr`. Suggested: `httr`, `testthat` (≥3.0.0), `knitr`, `rmarkdown`. R ≥ 4.0.0 required.

Keep the dependency footprint small — this is a CRAN package, and new hard
dependencies need a good reason.

## Repository conventions

- Documentation is roxygen2 with markdown enabled (`Roxygen: list(markdown = TRUE)`).
- Vignettes live in `vignettes/`; several are excluded from the build via
  `.Rbuildignore` because they are slow or need optional packages.
- Non-standard top-level files must be listed in `.Rbuildignore` or
  `R CMD check --as-cran` reports a NOTE.
- User-visible changes belong in `NEWS.md`.
