# Functions for getting information about a model

PKPDsim models encode information about using the model that can be
helpful for working with the model. This family of functions provides an
easier API for accessing useful information. See also
`attributes(model)` for less commonly used model metadata. Functions
will return `NULL` if the requested field is not available.

## Usage

``` r
get_model_parameters(model)

get_model_covariates(model)

get_model_fixed_parameters(model)

get_model_structure(model)

get_model_linearity(model)

get_model_auc_compartment(model)

get_model_iov(model)
```

## Arguments

- model:

  PKPDsim model

## Value

get_model_parameters: returns a vector of PK parameter names

get_model_covariates: returns a vector of covariate names

get_model_fixed_parameters: returns a vector of names of parameters that
are not associated with inter-individual or inter-occasion variability.

get_model_structure: returns a single string indicating model structure.
E.g.,: "1cmt_iv", "2cmt_oral".

get_model_linearity: returns a single string indicating model linearity.
E.g., "linear" or "nonlinear".

get_model_auc_compartment: returns the index of the final compartment,
which is conventionally the AUC compartment. Note: will not detect if
the final compartment is actually encoded to describe AUC.

get_model_iov: returns information about the IOV structure. For models
without IOV, returns a single field (`list(n_bins = 1)`). Models with
IOV will return additional fields: n_bins, bin durations, and CV
associated with each PK parameter.
