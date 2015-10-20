#' Parse regimen
#' @param regimen regimen
#' @param t_max t_max
#' @param t_obs t_obs
#' @param t_tte t_tte
#' @param p parameters
#' @param covariates covariates
#' @export
parse_regimen <- function(regimen, t_max, t_obs, t_tte, p, covariates) {

  if(length(regimen$t_inf) < length(regimen$dose_times)) {
    regimen$t_inf <- c(regimen$tinf, rep(tail(regimen$t_inf, 1), (length(regimen$dose_times) - length(regimen$t_inf))) )
  }

  ## first, add covariates to regiment to be incorproated in design
  if(!is.null(covariates)) {
    covt <- c()
    for (i in 1:length(covariates)) {
      covt <- data.frame(rbind(covt,
                               cbind(name = names(covariates)[i],
                                     time = covariates[[i]]$times,
                                     value = covariates[[i]]$value)))
      covt$time <- as.numeric(as.character(covt$time))
      covt$value <- as.numeric(as.character(covt$value))
    }
    # add covariate update times as dummy dose
    regimen$dose_times <- c(regimen$dose_times, covt$time)
    regimen$dose_amts <- c(regimen$dose_amts, rep(0, length(covt$time)))
    regimen$t_inf <- c(regimen$t_inf, rep(0, length(covt$time)))
    regimen$t_inf <- c(regimen$t_inf, rep(0, length(covt$time)))
    ord <- order(regimen$dose_times)
    regimen$dose_times <- regimen$dose_times[ord]
    regimen$dose_amts  <- regimen$dose_amts[ord]
    regimen$t_inf  <- regimen$t_inf[ord]
  }

  # parse list to a design (data.frame)
  if(regimen$type == "infusion") {
    design <- data.frame(rbind(cbind(t=regimen$dose_times, dose = regimen$dose_amts, dum = 0, t_inf = regimen$t_inf, rate = regimen$dose_amts / regimen$t_inf),
                               cbind(t=regimen$dose_times + regimen$t_inf, dose=0, dum = 1, t_inf = 0, rate = 0 ))) %>%
      dplyr::arrange(t, -dose)
  } else {
    design <- data.frame(rbind(cbind(t=regimen$dose_times, dose = regimen$dose_amts, dum = 0, t_inf = 0, rate = 0))) %>%
      dplyr::arrange(t, -dose)
  }
  if(!is.null(t_obs) && length(t_obs) != 0) { # make sure observation times are in dataset
    t_diff <- setdiff(t_obs, design$t)
    if(length(t_diff) > 0) {
      design <- data.frame(rbind(design, cbind(t = t_diff, dose = 0, dum = 0, t_inf = 0, rate = 0))) %>% arrange(t, -dose)
    }
  }
  if(!is.null(t_tte) && length(t_obs) != 0) { # make sure tte times are in dataset
    t_diff <- setdiff(t_tte, design$t)
    if(length(t_diff) > 0) {
      design <- data.frame(rbind(design, cbind(t = t_diff, dose = 0, dum = 0, t_inf = 0, rate = 0))) %>% arrange(t, -dose)
    }
  }
  if (is.null(t_max)) {
    if (length(design$t) > 1) {
      t_max <- tail(design$t,1) + max(diff(design$t))
    } else {
      t_max <- tail(design$t,1) + 24 # guess timeframe, user should use tmax argument
    }
    if(!is.null(t_obs) && length(t_obs) > 0) {
      if(is.null(t_max) || is.na(t_max) || t_max < max(t_obs)) { t_max <- max(t_obs) }
    }
    if(!is.null(t_tte) && length(t_tte) > 0) {
      if(is.null(t_tte) || is.na(t_max) || t_max < max(t_tte)) { t_max <- max(t_tte) }
    }
  }
  design <- rbind(design %>%
                    dplyr::filter(t < t_max), tail(design,1))
  if(!is.null(p$F) && class(p$F) == "numeric") {
    design$dose <- design$dose * p$F
  }
  design[length(design[,1]), c("t", "dose")] <- c(t_max,0)

  # now add the covariate values to the design dataset
  if(!is.null(covariates)) {
    for(i in length(covariates)) {
      design[[paste0("cov_", names(covariates)[i])]] <- 0
    }
    for(i in 1:length(covt[,1])) {
      ## using LOCF
      design[design$t >= covt[i,]$time, c(paste0("cov_", covt[i,]$name))] <- covt[i,]$value
    }
  }
  design <- design[!duplicated(paste0(design$t,design$dose,design$dum)),]
  # remove covariate points where there is also a dose
  if(!is.null(covariates)) {
    design <- design[!(design$t %in% covt$time & design$t %in% regimen$dose_times & design$dose == 0),]
  }
  return(design)
}
