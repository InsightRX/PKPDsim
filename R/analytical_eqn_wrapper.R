#' Wrapper for using analytical equations with PKPD regimens
#'
#' In development. Needs to be optimized significantly to be useful in production.
#'
#' @param analytical analytical equation, taking parameters `amt`, `parameters`, and `t`, and returning a vector of values for `y`
#' @param design design dataset created by `sim_ode`
#' @param parameters list of parameters
#'
analytical_eqn_wrapper <- function(analytical, design = NULL, parameters) {
  doses <- which(design$evid == 1)
  dat <- data.frame(time = design$t, y = 0)
  scale <- parameters$V
  for (i in 1:length(doses)) {
    if(i < length(doses)) {
      i_end <- doses[i+1]
      if(i > 1) {
        amt <- dat$y[doses[i]] * scale + design$dose[doses[i]]
        t <- design$t[doses[i]:i_end] - design$t[doses[i]]
      } else {
        amt <- design$dose[doses[i]]
        t <- design$t[doses[i]:i_end]
      }
    } else {
      i_end <- length(design$t)
      amt <- dat$y[doses[i]] * scale + design$dose[doses[i]]
      t <- design$t[doses[i]:i_end] - design$t[doses[i]]
    }
    y <- analytical(amt, parameters, t)
    dat[doses[i]:i_end,]$y <- y
  }
  dat <- data.frame(dat)
  dat$time <- as.num(dat$time)
  dat$y <- as.num(dat$y)
  dat$obs <- as.num(dat$y)
  class(dat) <- c("PKPDsim_data", class(dat))
  return(dat)
}
