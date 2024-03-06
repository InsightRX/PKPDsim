#' Convenience function to calculate the AUC based on PK model parameters at
#' any given moment, for linear iv models.
#'
#' @param f analytic model to use, show available models using `advan()`
#' @param regimen PKPDsim regimen created using `new_regimen`. Not required,
#' regimen can also be specified using `dose`, `interval`, and `t_inf`.
#' @param dose dosing amount. Only used if no `regimen` supplied.
#' @param interval dosing interval. Only used if no `regimen` supplied.
#' @param t_inf infusion length. Only used if no `regimen` supplied.
#' @param parameters list of parameter estimates. Requires CL/V for
#' 1-compartment models, CL/V/Q/V2 for 2-compartment models, and CL/V/Q/V2/Q2/V3
#' for 3-compartment models.
#' @param t_obs vector of observation times for AUC
#' @param ... optional arguments passed to `advanc_create_data()`
#'
#' @returns a data.frame with `t` and `auc`
#'
#' @examples
#' dat <- calc_auc_analytic(
#'   f = "2cmt_iv_infusion",
#'   regimen = new_regimen(
#'     amt = 1000, n = 10, type = "infusion",
#'     t_inf = 1, interval = 24
#'   ),
#'   parameters = list(CL = 5, V = 50, Q = 8, V2 = 150)
#' )
#'
#' @export
#'
calc_auc_analytic <- function(
    f = c(
      "1cmt_iv_infusion", "2cmt_iv_infusion", "3cmt_iv_infusion",
      "1cmt_iv_bolus", "2cmt_iv_bolus", "3cmt_iv_bolus"
    ),
    regimen = NULL,
    dose = NULL,
    interval = NULL,
    t_inf = NULL,
    parameters,
    t_obs = c(0, 24, 48, 72),
    ...
) {
  model <- match.arg(f)

  ## make sure model and regimens match up
  ## basically, if regimen is all bolus, then use bolus model
  ## if 1 or more doses are infusion, then make sure to use infusion model.
  if(!is.null(regimen)) {
    if(any(regimen$type == "infusion")) {
      model <- gsub("bolus", "infusion", model)
      if(any(regimen$type != "infusion")) {
        regimen$type <- "infusion"
        regimen$t_inf[regimen$type != "infusion"] <- 1e-6
      }
    } else {
      model <- gsub("infusion", "bolus", model)
    }
  } else {
    if(is.null(dose) || is.null(interval)) {
      stop("Specify `regimen` or `dose` and `interval`")
    }
    if(is.null(t_inf) || t_inf == 0) t_inf <- 1e-6
    regimen <- new_regimen(
      amt = dose,
      type = "infusion",
      interval = interval,
      t_inf = t_inf,
      n = round(max(t_obs) / interval) + 1
    )
  }

  ## Create simulation template and run model
  cmts <- as.numeric(substr(model, 1, 1)) +
    ifelse(length(grep("oral", model)) > 0, 1, 0) # if oral, add 1 compartment
  t_obs_0 <- unique(c(0, t_obs))
  data_template <- advan_create_data(
    regimen = regimen,
    parameters = parameters,
    t_obs = unique(c(0, t_obs_0)),
    cmts = cmts,
    ...
  )
  advan_model <- advan(model, cpp = TRUE)

  ## Parse output
  res <- advan_model(data_template)
  res <- res[res$AMT == 0 & res$TIME %in% t_obs_0,]
  tmp <- data.frame(
    t = res$TIME,
    y = res$DV,
    auc = c(0, diff(res$AUC))
  )

  ## Parse and return
  tmp <- tmp[!duplicated(tmp$t) & tmp$t %in% t_obs, c("t", "y", "auc")]
  merge( # Merge makes sure it works correctly when `t_obs` has duplicates
    data.frame(t = t_obs),
    tmp,
    by = "t"
  )
}
