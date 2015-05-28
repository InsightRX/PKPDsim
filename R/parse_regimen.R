#' @export
parse_regimen <- function(regimen, t_max, t_obs, t_tte, p, covariates) {

  ## first, add covariates to regiment to be incorproated in design
  if(!is.null(covariates)) {
    covt <- c()
    for (i in 1:length(covariates)) {
      covt <- data.frame(rbind(covt, cbind(name = names(covariates)[i],
                                time = covariates[[i]]$times,
                                value = covariates[[i]]$value)))
      covt$time <- as.numeric(as.character(covt$time))
      covt$value <- as.numeric(as.character(covt$value))
    }
    # add covariate update times as dummy dose
    regimen$dose_times <- c(regimen$dose_times, covt$time)
    regimen$dose_amts <- c(regimen$dose_amts, rep(0, length(covt$time)))
    ord <- order(regimen$dose_times)
    regimen$dose_times <- regimen$dose_times[ord]
    regimen$dose_amts  <- regimen$dose_amts[ord]
  }

  # parse list to a design (data.frame)
  if(regimen$type == "infusion") {
    design <- data.frame(rbind(cbind(t=regimen$dose_times, dose = regimen$dose_amts, dum = 0),
                               cbind(t=regimen$dose_times + regimen$t_inf, dose=0, dum = 1))) %>%
      dplyr::arrange(t)
  } else {
    design <- data.frame(rbind(cbind(t=regimen$dose_times, dose = regimen$dose_amts, dum = 0)))
  }
  if(!is.null(t_obs) && length(t_obs) != 0) { # make sure observation times are in dataset
    design <- data.frame(rbind(design, cbind(t = setdiff(t_obs, design$t), dose = 0, dum = 0))) %>% arrange(t)
  }
  if(!is.null(t_tte) && length(t_obs) != 0) { # make sure tte times are in dataset
    design <- data.frame(rbind(design, cbind(t = setdiff(t_tte, design$t), dose = 0, dum = 0))) %>% arrange(t)
  }
  if (is.null(t_max)) {
    if (length(design$t) > 1) {
      t_max <- tail(design$t,1) + max(diff(design$t))
    } else {
      t_max <- tail(design$t,1) + 24 # guess timeframe, user should use tmax argument
    }
    if(!is.null(t_obs) && length(t_obs) > 0) {
      if(is.null(t_max) || t_max < max(t_obs)) { t_max <- max(t_obs) }
    }
    if(!is.null(t_tte) && length(t_tte) > 0) {
      if(is.null(t_tte) || t_max < max(t_tte)) { t_max <- max(t_tte) }
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
  return(design)
}
