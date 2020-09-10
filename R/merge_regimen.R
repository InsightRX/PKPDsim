#' Merge two regimens together.
#'
#' In contrast to `join_regimen`, which joins two consecutive regimens together, `merge_regimen` merges two or
#' more regimens given at the same time. This can e.g. be used to define regimens for multi-drug models.
#'
#' @param regimens List of PKPDsim regimens created with `new_regimen`.
#'
#' @export
merge_regimen <- function(regimens) {
  reg <- data.frame(regimens[[1]])
  reg$type <- as.character(reg$type)
  if(length(regimens) > 1) {
    for(i in 2:length(regimens)) {
      reg_tmp <- data.frame(regimens[[i]])
      reg_tmp$type <- as.character(reg_tmp$type)
      reg_tmp$t_inf <- ifelse0(0, reg_tmp$t_inf)
      reg <- rbind(reg, reg_tmp[,c("interval", "n", "type", "t_inf", "dose_times", "dose_amts", "first_dose_time")])
    }
  }
  reg <- reg[order(reg$dose_times, decreasing = FALSE),]
  class(reg) <- c("regimen", "list")
  return(reg)
}
