#' @export
num_int_wrapper <- function (times, A_init, des, p_ind, lsoda_func, cpp, step_size) {
  if(cpp) {
    des_out <- sim_cpp(A_init, c(min(times), max(times)), list(test="test"), step_size)
  } else {
    des_out <- lsoda_func(A_init, times, des, p_ind)
  }
  dat_ind <- c()
  for (j in 1:length(A_init)) {
    dat_ind <- rbind (dat_ind, cbind(t=des_out[,1], comp=j, y=des_out[,(j+1)]))
  }
  return(data.frame(dat_ind))
}
