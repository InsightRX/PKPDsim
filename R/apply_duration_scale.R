#' Apply infusion duration scale to a regimen
#'
#' E.g. see Centanni et al. Clin Pharmacokinet 2024. An estimated scaling factor
#' for the length of the infusion was applied there in a model for vincristine.
#' This is likely most relevant for very short infusions.
#'
#' Implementation is similar to handling of `lagtime`, i.e. the regimen that is the
#' input for the simulation function is updated.
#'
#' @param regimen PKPDsim regimen
#' @param duration_scale infusion length scale.
#' @param parameters parameter list, required if the duration scale is
#' specified as a parameter.
#' @param cmt_mapping map of administration types to compartments, e.g.
#' `list("oral" = 1, "infusion" = 2, "bolus" = 2)`.
#'
#' @export
#'
#' @return Original regimen with infusion lengths scaled by a factor
#'
apply_duration_scale <- function(
  regimen,
  duration_scale = NULL,
  parameters = NULL,
  cmt_mapping = NULL
) {
  if(is.null(regimen$t_inf) || all(regimen$t_inf == 0)) {
    warning("`duration_scale` is only relevant for infusion regimens with an infusion length > 0.")
    return(regimen)
  }
  if(!is.null(duration_scale)) {
    if(class(duration_scale) %in% c("numeric", "integer")) {
      if(length(duration_scale) == 1) {
        regimen$t_inf <- regimen$t_inf * duration_scale
      } else {
        regimen$t_inf <- regimen$t_inf * duration_scale[regimen$cmt]
      }
    } else if(class(duration_scale) %in% c("character")) {
      if(is.null(regimen$cmt)) {
        if(!is.null(cmt_mapping)) {
          regimen$cmt <- as.numeric(cmt_mapping[regimen$type])
        } else {
          regimen$cmt <- rep(1, length(regimen$dose_times))
        }
      }
      if(length(duration_scale) == 1) {
        regimen$t_inf <- regimen$t_inf * parameters[[duration_scale]]
      } else {
        regimen$t_inf <- regimen$t_inf * as.numeric(unlist(parameters[duration_scale[regimen$cmt]]))
      }
    }
  } else {
    warning("Please specify `duration_scale` to apply to regimen.")
  }
  return(regimen)
}
