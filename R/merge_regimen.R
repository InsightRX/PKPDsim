#' Merge two regimens together.
#' 
#' In contrast to `join_regimen`, which joins two consecutive regimens together, `merge_regimen` merges two or
#' more regimens given at the same time. This can e.g. be used to define regimens for multi-drug models.
#' 
#' @param regimen List of PKPDsim regimens created with `new_regimen`.
#'
#' @export
merge_regimen <- function(regimens) {
  reg <- data.frame(regimens[[1]]) %>% dplyr::mutate(type = as.character(type))
  if(length(regimens) > 1) {
    for(i in 2:length(regimens)) {
      reg <- dplyr::bind_rows(reg, data.frame(regimens[[i]]) %>% dplyr::mutate(type = as.character(type)))
    }
  }
  reg <- reg %>% 
    dplyr::arrange(dose_times)
  class(reg) <- c("regimen", "list")
  return(reg)
}
