#' Apply lagtime to a regimen
#'
#' @param regimen PKPDsim regimen
#' @param lagtime lagtime object, either single value / parameter name or vector of values/parameter names for all compartments.
#' @param parameters parameter list, required if parameters are specified.
#'
#' @export
apply_lagtime <- function(regimen, lagtime, parameters) {
    if(class(lagtime) %in% c("numeric", "integer")) {
      if(length(lagtime) == 1) {
        regimen$dose_times <- regimen$dose_times + lagtime
      } else {
        regimen$dose_times <- regimen$dose_times + lagtime[regimen$cmt]
      }
    }
    if(class(lagtime) %in% c("character")) {
      if(length(lagtime) == 1) {
        regimen$dose_times <- regimen$dose_times + parameters[[lagtime]]
      } else {
        par_tmp <- parameters
        par_tmp[["0"]] <- 0
        regimen$dose_times <- regimen$dose_times + as.numeric(unlist(par_tmp[lagtime[regimen$cmt]]))
      }
    }
    regimen <- regimen %>% data.frame() %>% dplyr::arrange(dose_times) 
    class(regimen) <- c("regimen", "list")
    return(regimen)
}
