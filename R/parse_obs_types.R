#' Parse observation types to simulation code
#'
#' @param obs specified observation object including at least a description of
#'       which variable(s) are associated with a particular compartment, e.g.
#'       `list(variable="CONC", scale="1")`.
#' @param initial is this for the initial code block in the C++ template that
#'       initializes the variables and compartments (`TRUE`), or for the
#'       second code block used for the rest of the dataset?
#'
#' @keywords internal
parse_obs_types <- function(obs, initial = FALSE) {
  if(length(obs$variable) == 1) {
    tmp <- paste0("      obs.insert(obs.end(), ", obs$variable, "); ")
  } else {
    tmp <- c()
    for(i in 1:length(obs$variable)) {
      str_else <- ifelse(length(obs$variable)-i <= 0, str_else <- "else", "else if")
      str_if <- ifelse(i == 1, "if", "  ")
      tmp <- c(
        tmp,
        paste0(
          "      ", str_if, " (obs_type[i+", ifelse(initial, 0, 1),
          "]==", i, ") { obs.insert(obs.end(), ", obs$variable[i], "); } ",
          str_else, " "
        )
      )
    }
    # make sure something is pushed on obs stack
    tmp <- c(
      tmp,
      paste0("         { obs.insert(obs.end(), ", obs$variable[1], "); }")
    )
  }
  return(paste0(tmp, collapse = "\n"))
}

#' Checks obs input for valid combinations of cmt, var, scale
#'
#' @inheritParams parse_obs_types
check_obs_input <- function(obs) {
  if(!is.null(obs$scale) && !is.null(obs$variable)) {
    stop(
      "obs should contain only one of `scale` or `variable`.",
      call. = FALSE
    )
  }
  if (is.null(obs$scale)) obs$scale <- 1
  if (is.null(obs$cmt))   obs$cmt <- 1

  if (length(obs$scale) > 1 && length(obs$scale) != length(obs$cmt)) {
    stop(
      "obs$scale should be either the same length as obs$cmt, or 1",
      call. = FALSE
    )
  }

  if (length(obs$scale) == 1 && length(obs$cmt) > 1) {
    obs$scale <- rep(obs$scale, length(obs$cmt))
  }
  obs
}
