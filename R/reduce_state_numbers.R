#' R starts counting vector indices at 1, c++ starts at 0,
#' so reduce all state numbers in the Cpp function definition by 1
reduce_state_numbers <- function (ode_def) {
  m <- gregexpr("\\[([0-9])\\]", ode_def)
  m1 <- unlist(regmatches(ode_def, m))
  m2 <- unlist(regmatches(ode_def, m, invert = TRUE))
  for (i in seq(m1)) {
    tmp <- as.numeric(gsub("[^0-9]", "", m1[i]))
    m1[i] <- paste0("[", tmp-1, "]")
  }
  return(paste0(c(m2[1], t(cbind(m1,m2[-1]))), collapse=""))
}
