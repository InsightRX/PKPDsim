#' Get fixed parameters
#'
#' Get fixed parameters listed in model definition if present. If not present, use size of omega matrix to determine fixed parameters.
#'
#' @param def Model definition as output by [read_model_json()]
#' @md
get_fixed_parameters <- function(def) {
  # if fixed parameters are defined as null, i.e. written in the JSON as
  # `"fixed": null`, then we still want to use that, so that's why this isn't
  # written as `if (!is.null(def$fixed))`
  if ("fixed" %in% names(def)) {
    return(def$fixed)
  }
  n_estimated <- lower_triangle_mat_size(def$omega)
  names(tail(def$parameters, -n_estimated))
}
