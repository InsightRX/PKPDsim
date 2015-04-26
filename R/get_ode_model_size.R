# get the number of states in the ODE from the code
#' @export
get_ode_model_size <- function(code) {
  m <- gregexpr("\\[([0-9])\\]", code)
  m1 <- unlist(regmatches(code, m))
  for (i in seq(m1)) {
    tmp <- as.numeric(gsub("[^0-9]", "", m1[i]))
  }
  return(max(as.numeric((tmp))))
}

