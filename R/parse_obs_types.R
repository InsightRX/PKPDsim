#' Parse observation types to simulation code
#'
#' @param obs specified observation object including at least a description of 
#'       which variable(s) are associated with a particular compartment, e.g. 
#'       `list(variable="CONC", scale="1")`.
parse_obs_types <- function(obs) {
  if(is.null(obs$scale)) obs$scale <- "1.0"
  if(length(obs$variable) == 1) {
    tmp <- paste0("      obs.insert(obs.end(), ", obs$variable, "/(", obs$scale,")); ")
  } else {
    if(length(obs$scale) < length(obs$variable)) {
      obs$scale <- rep(obs$scale[1], length(obs$variable))
      warning("Provided `scale` vector not same length as `variable` vector.")
    }
    tmp <- c()
    for(i in 1:length(obs$variable)) {
      str_else <- ifelse(length(obs$variable)-i <= 0, str_else <- "else", "else if")
      str_if <- ifelse(i == 1, "if", "  ")
      tmp <- c(
        tmp,
        paste0(
          "      ", str_if, " (obs_type[i+1]==", i, ")", 
          " { obs.insert(obs.end(), ", obs$variable[i], "/(", obs$scale[i], ")); } ", 
          str_else, " "
        )
      )
    }
    # make sure something is pushed on obs stack
    tmp <- c(
      tmp, 
      paste0("         { obs.insert(obs.end(), ", obs$variable[1], "/(", obs$scale[1], ")); }")
    ) 
  }
  return(paste0(tmp, collapse = "\n"))
}
