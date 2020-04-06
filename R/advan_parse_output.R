#' Internal function to parse the raw output from ADVAN-style functions
#'
#' @param data simulation output data
#'
#' @export
advan_parse_output <- function(data, cmts = 1) {
  cmts <- PKPDsim::ifelse0(cmts, 1)
  out <- data[, c("ID", "TIME", "DV", paste0("A", 1:cmts))]
  names(out) <- c("id", "t", "y", paste0("A", 1:cmts))
  data$obs_type <- 1
  return(out)
}
