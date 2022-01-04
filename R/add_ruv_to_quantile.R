#' Calculate the increase in a specific quantile for a distribution on y when residual variability is added
#'
#' @param y y with
#' @param sd_y standard deviation of y without residual variability added. Will add normally distributed variability (potetially on log-scale).
#' @param log_scale add variability on log scale (FALSE by default, DEPRECATED!).
#' @param q quantile
#' @param ruv list of residual variability (`prop` and `add`)
#' @param ... passed arguments
#' 
#' @export
#' @return Numeric vector of y values with residual variability
add_ruv_to_quantile <- function(
  y,
  sd_y,
  log_scale = FALSE,
  q = NULL,
  ruv = list(),
  ...) {
  sd_new <- sqrt(sd_y^2 + (ruv$prop * y)^2 + ruv$add^2)
  if(!is.null(q)) {
    return(qnorm(p = q, mean = y, sd = sd_new))
  } else {
    return(sd_new)
  }
}
