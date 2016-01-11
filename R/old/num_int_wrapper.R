#' Wrapper for deSolve
#' @param times time vector
#' @param A_init vector of initial state
#' @param des ODE function
#' @param p_ind parameters
#' @param lsoda_func LSODA function
#' @export
num_int_wrapper <- function (times, A_init, des, p_ind, lsoda_func) {
  des_out <- lsoda_func(A_init, times, des, p_ind)
  dat_ind <- c()
  for (j in 1:length(A_init)) {
    dat_ind <- rbind (dat_ind, cbind(t=des_out[,1], comp=j, y=des_out[,(j+1)]))
  }
  return(data.frame(dat_ind))
}
