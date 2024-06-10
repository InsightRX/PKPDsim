#' Functions for getting information about a model
#'
#' PKPDsim models encode information about using the model that can be helpful
#' for working with the model. This family of functions provides an easier API
#' for accessing useful information. See also `attributes(model)` for less
#' commonly used model metadata. Functions will return `NULL` if the requested
#' field is not available.
#'
#' @name get_model_info
#' @param model PKPDsim model
NULL

#' Get parameters required for a model
#' @rdname get_model_info
#' @returns get_model_parameters: returns a vector of PK parameter names
#' @export
get_model_parameters <- function(model) attr(model, "parameters")

#' Get covariates required for a model
#' @rdname get_model_info
#' @returns get_model_covariates: returns a vector of covariate names
#' @export
get_model_covariates <- function(model) attr(model, "covariates")

#' Get fixed parameters in model
#' @rdname get_model_info
#' @returns get_model_fixed_parameters: returns a vector of names of parameters
#'   that are not associated with inter-individual or inter-occasion
#'   variability.
#' @export
get_model_fixed_parameters <- function(model) attr(model, "fixed")

#' Get model structure
#' @rdname get_model_info
#' @returns get_model_structure: returns a single string indicating model
#'   structure. E.g.,: "1cmt_iv", "2cmt_oral".
#' @export
get_model_structure <- function(model) attr(model, "misc")[["model_type"]]

#' Get model linearity
#' @rdname get_model_info
#' @returns get_model_linearity: returns a single string indicating model
#'   linearity. E.g., "linear" or "nonlinear".
#' @export
get_model_linearity <- function(model) attr(model, "misc")[["linearity"]]

#' Get AUC compartment
#' @rdname get_model_info
#' @returns get_model_auc_compartment: returns the index of the final
#'   compartment, which is conventionally the AUC compartment. Note: will not
#'   detect if the final compartment is actually encoded to describe AUC.
#' @export
get_model_auc_compartment <- function(model) attr(model, "size")

#' Get inter-occasion variability specifications
#' @rdname get_model_info
#' @returns get_model_iov: returns information about the IOV structure. For
#'   models without IOV, returns a single field (`list(n_bins = 1)`). Models
#'   with IOV will return additional fields: n_bins, bin durations, and CV
#'   associated with each PK parameter.
#' @export
get_model_iov <- function(model) attr(model, "iov")
