#' Get the number of states in the ODE from the code
#' code C++ code for model
#' @param code C++ code
#' @export
get_ode_model_size <- function(code) {
  m <- gregexpr("\\[([0-9])\\]", code)
  m1 <- unlist(regmatches(code, m))
  tmp <- 0
  for (i in seq(m1)) {
    tmp <- as.numeric(gsub("[^0-9]", "", m1[i]))
  }
  return(max(as.numeric((tmp))))
}
