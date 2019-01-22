#' Simulate analytical PK equations
#' WORK IN PROGRESS!!!
#'
#' @param model model, choose from `pk_1cmt_iv`, `pk_1cmt_oral`, `pk_2cmt_iv`, `pk_2cmt_oral`.
#' @param parameters list of model parameters
#' @param regimen PKPDsim regimen
#' @param t_obs vector of observation times
#' @export
sim_ae <- function(
  model = "pk_1cmt_iv",
  parameters,
  regimen,
  cov_model = NULL,
  covariates = NULL,
  t_obs = NULL
) {
  if(is.null(t_obs)) {
    t_obs <- 1:72
  }
  tmp <- c()
  t_obs <- unique(c(regimen$dose_times, regimen$dose_times + regimen$t_inf, t_obs))
  t_obs <- t_obs[order(t_obs)]
  dat <- data.frame(t = t_obs, comp = "obs", y = 0)

  ## store initial values for parameters
  if(!is.null(covariates) && !is.null(cov_model)) {
    tmp <- list()
    # put covariates in the grid
    for(key in names(covariates)) {
      tmp <- covariates[[key]]
      dat[[key]] <- NA
      for(j in 1:length(tmp$times)) {
        dat[dat$t >= tmp$times[j],][[key]] <- tmp$value[j]
      }
      if(tmp$implementation == "interpolate") { # not exactly the same 'interpolation' as in ODE, as the mean will be taken over a dosing interval
        t_unq_idx <- !duplicated(dat[[key]])
        t_unq <- dat$t[t_unq_idx]
        val_unq <- dat[[key]][t_unq_idx]
        if(length(t_unq) > 1) {
          for(j in 1:(length(t_unq)-1)) {
            alpha <- (val_unq[j+1] - val_unq[j]) / (t_unq[j+1] - t_unq[j])
            sel <- dat$t >= t_unq[j] & dat$t < t_unq[j+1]
            dat[sel,][[key]] <- val_unq[j] + alpha * (dat[sel,]$t - t_unq[j])
          }
        }
      }
    }

    # loop over parameters
    for(key in names(parameters)) {
      dat[[key]] <- parameters[[key]]
    }
    for(j in 1:length(cov_model)) {
      lh <- stringr::str_replace_all(stringr::str_split(cov_model[j], "=")[[1]][1], " ", "")
      dat[[lh]] <- dat %>% mutate_(par = cov_model[j]) %>% .$par
    }
  }

  models <- list(
    "pk_1cmt_iv" = "pk_1cmt_inf",
    "pk_1cmt_oral" = "pk_1cmt_oral",
    "pk_2cmt_iv" = "pk_2cmt_inf",
    "pk_2cmt_oral" = "pk_2cmt_oral",
    "advan1" = "pk_1cmt_inf",
    "advan2" = "pk_1cmt_oral",
    "advan3" = "pk_2cmt_inf",
    "advan4" = "pk_2cmt_oral"
  )
  if(model %in% names(models)) {
    f <- get(models[[model]], asNamespace("clinPK"))
  } else {
    stop(paste0("Please choose from the following models: ", paste0(names(models), collapse = ", ")))
  }
  for(i in 1:length(regimen$dose_times)) {
    t_dos1 <- regimen$dose_times[i]
    sel <- dat$t > t_dos1
    dat[sel,]$y <- dat[sel,]$y + f(
      t = t_obs[sel] - t_dos1,
      dose = regimen$dose_amts[i],
      t_inf = ifelse(regimen$t_inf[i] == 0, 1e-9, regimen$t_inf[i]),
      tau = 1e6, # avoid repeating
      CL = mean(dat$CL[sel]),
      V = mean(dat$V[sel]))$dv
  }
  return(dat)
}

#
# library(PKPDsim)
# library(dplyr)
# library(ggplot2)
# library(PKPDplot)
# reg <- new_regimen(amt = rep(1000,4),
#                    times = c(0, 12, 24, 36),
#                    type = "infusion",
#                    t_inf = 1)
# par <- list(CL = 5, V = 50)
# covs <- list(WT = new_covariate(70), CR = new_covariate(value = c(3, .5), times = c(0, 24)))
# cov_model <- list(
#   CL = function(parameter, covariates) {
#     parameter * (covariates$WT / 70)^0.75 * (1/covariates$CR)^0.5
#   },
#   V = function(parameter, covariates) {
#     parameter * (covariates$WT / 70)
#   }
# )
# cov_model <- c(
#   "CL = CL * (WT/70)**0.75 * (1/CR)^0.5",
#   "V = V * WT/70"
# )
# res <- sim_ae(model = "pk_1cmt_iv",
#        cov_model = cov_model,
#        covariates = covs,
#        regimen = reg, parameters = par)
# res %>%
#   ggplot(aes(x = t, y = y)) + geom_line()
