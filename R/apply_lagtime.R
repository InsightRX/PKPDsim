#' Apply lagtime to a regimen
#'
#' @param regimen PKPDsim regimen
#' @param lagtime lagtime object, either single value / parameter name or vector of values/parameter names for all compartments.
#' @param parameters parameter list, required if parameters are specified.
#' @param cmt_mapping map of administration types to compartments, e.g. `list("oral" = 1, "infusion" = 2, "bolus" = 2)`.
#'
#' @export
#' @return Original regimen with lagtime added to dose times
apply_lagtime <- function(
  regimen,
  lagtime,
  parameters,
  cmt_mapping = NULL
) {
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
        if(is.null(regimen$cmt)) {
          if(!is.null(cmt_mapping)) {
            regimen$cmt <- as.numeric(cmt_mapping[regimen$type])
          } else {
            regimen$cmt <- rep(1, length(regimen$dose_times))
          }
        }
        par_tmp <- parameters
        par_tmp[["0"]] <- 0
        regimen$dose_times <- regimen$dose_times + as.numeric(unlist(par_tmp[lagtime[regimen$cmt]]))
      }
    }
    return(regimen)
}
