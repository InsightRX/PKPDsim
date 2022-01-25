#' Transform a vector into a string that evaluates to the same vector
#'
#' Collapses a vector into a comma-separated list with strings quoted
#' (and special characters escaped). A general purpose helper function
#' for writing new model code.
#'
#' @param vec a vector
#' @param return_null logical indicating if `vec` is NULL whether to "NULL"
#'   (alternative is to return an empty list).
#' @return character string of length 1

vector_to_R_code <- function(vec, return_null = TRUE) {
  if (return_null && is.null(vec)) return("NULL")
  paste0("c(", paste(add_quotes(vec), collapse = ", "), ")")
}

#' Transforms bioavailability specs into appropriate R code
#'
#' Specialized wrapper around `vector_to_R_code` that makes reasonable PK
#' assumptions for when the bioavailability specification is NULL.
#'
#' @param bioav bioavailability specification, either NULL (assume a value of 1
#'   in all compartments), a single value (assume it applies to all
#'   compartments), or a vector of values.
#' @return character string of length 1

bioavailability_to_R_code <- function(bioav) {
  if (is.null(bioav)) bioav <- "1"

  if(class(bioav) == "character" || length(bioav) > 1) {
    vector_to_R_code(bioav)
  } else {
    bioav
  }

}
