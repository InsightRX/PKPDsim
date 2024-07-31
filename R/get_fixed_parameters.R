#' Get fixed parameters from model definition.
#'
#' Get fixed parameters listed in model definition. This function is used when
#' parsing model specifications before the model has been compiled. Please see
#' `[get_model_fixed_parameters]` for accessing fixed parameters from a model
#' that has already been built.
#'
#' @param def Model definition as output by [read_model_json()]
#' @md
get_fixed_parameters <- function(def) {
  # if fixed parameters are defined as null, i.e. written in the JSON as
  # `"fixed": null`, then we still want to use that, so that's why this isn't
  # written as `if (!is.null(def$fixed))`
  if ("fixed" %in% names(def)) {
    return(def$fixed)
  } else {
    stop(
      "Fixed parameters are required to be present in the model definition.",
      call. = FALSE
    )
  }
}
