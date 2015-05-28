#' @export
parse_regimen <- function(regimen, t_max, t_obs, t_tte, p) {
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
  return(design)
}
