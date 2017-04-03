#' Parse regimen
#' @param regimen regimen
#' @param t_max t_max
#' @param t_obs t_obs
#' @param t_tte t_tte
#' @param p parameters
#' @param covariates covariates
#' @param model model
#' @export
parse_regimen <- function(regimen, t_max, t_obs, t_tte, p, covariates, model = NULL) {

  if(length(regimen$t_inf) < length(regimen$dose_times)) {
    regimen$t_inf <- c(regimen$tinf, rep(utils::tail(regimen$t_inf, 1), (length(regimen$dose_times) - length(regimen$t_inf))) )
  }

  dose_cmt <- 1
  if(!is.null(regimen$cmt)) {
    regimen$dose_cmt <- regimen$cmt
  } else {
    if(!is.null(model)) {
      if(!is.null(attr(model, "dose")$cmt)) {
        dose_cmt <- attr(model, "dose")$cmt
      }
    }
    regimen$dose_cmt <- rep(dose_cmt, length(regimen$dose_times))
  }

  ## bioavailability
  bioav <- 1
  # Handled now in Cpp file!
  # if(!is.null(attr(model, "dose")$bioav)) {
  #   bioav <- attr(model, "dose")$bioav
  # }

  ## first, add covariates to regimen to be incorporated in design
  if(!is.null(covariates)) {
    covt <- c()
    for (i in 1:length(covariates)) {
      covt <- data.frame(rbind(covt,
                    data.frame(name = names(covariates)[i],
                          time = covariates[[i]]$times,
                          value = covariates[[i]]$value,
                          implementation = covariates[[i]]$implementation)))
      covt$time <- as.numeric(as.character(covt$time))
      if(class(covt$value) != "numeric") {
        # replace any factors / strings with their numeric index
        idx <- unique(as.character(covt$value))
        covt$value <- match(covt$value, idx)
      }
    }
    # add covariate update times as dummy dose
    regimen$dose_times <- c(regimen$dose_times, covt$time)
    regimen$dose_amts <- c(regimen$dose_amts, rep(0, length(covt$time)))
    regimen$type <- c(regimen$type, rep(0, length(covt$time)))
    regimen$dose_cmt <- c(regimen$dose_cmt, rep(0, length(covt$time)))
    regimen$t_inf <- c(regimen$t_inf, rep(0, length(covt$time)))

    ord <- order(regimen$dose_times)
    regimen$dose_times <- regimen$dose_times[ord]
    regimen$dose_amts  <- regimen$dose_amts[ord]
    regimen$type <- regimen$type[ord]
    regimen$dose_cmt  <- regimen$dose_cmt[ord]
    regimen$t_inf  <- regimen$t_inf[ord]
    regimen$evid <- 2
    regimen$bioav <- 0
  }

  # parse list to a design (data.frame)
  type <- (regimen$type == "infusion") * 1
  regimen$t_inf[type == 0] <- 0 # make sure inf_time is 0 for boluses
  dos <- data.frame(cbind(t = regimen$dose_times,
                  dose = regimen$dose_amts,
                  type = type,
                  dum = 0,
                  dose_cmt = regimen$dose_cmt,
                  t_inf = regimen$t_inf,
                  evid = 1,
                  bioav = bioav,
                  rate = 0))
  if(sum(regimen$t_inf > 0) > 0) {
    dos$rate[regimen$t_inf > 0] <- regimen$dose_amts[regimen$t_inf > 0] / regimen$t_inf[regimen$t_inf > 0]
  }
  if(any(regimen$type == "infusion")) {
    dos_t2 <- cbind(t = regimen$dose_times[regimen$type == "infusion"] + regimen$t_inf[regimen$type == "infusion"],
                          dose = 0,
                          type = 0,
                          dum = 1,
                          dose_cmt = regimen$dose_cmt[regimen$type == "infusion"],
                          t_inf = 0,
                          evid = 2,
                          bioav = 0,
                          rate = 0)
    dos[(length(dos[,1])+1) : (length(dos[,1])+length(dos_t2[,1])),] <- dos_t2
    dos <- data.frame(dos)
  }
  design <- dos[order(dos$t, -dos$dose),]
  if(!is.null(t_obs) && length(t_obs) != 0) { # make sure observation times are in dataset
    t_diff <- setdiff(t_obs, design$t)
    if(length(t_diff) > 0) {
      design[(length(design[,1])+1) : (length(design[,1])+length(t_diff)),] <- cbind(
         t = t_diff,
         dose = 0,
         type = 0,
         dum = 0,
         dose_cmt = 0,
         t_inf = 0,
         evid = 0,
         bioav = 0,
         rate = 0)
      design <- design[order(design$t, -design$dose),]
    }
  }
  if(!is.null(t_tte) && length(t_obs) != 0) { # make sure tte times are in dataset
    t_diff <- setdiff(t_tte, design$t)
    if(length(t_diff) > 0) {
      tmp <- cbind(
         t = t_diff,
         dose = 0,
         type = 0,
         dum = 0,
         dose_cmt = 0,
         t_inf = 0,
         evid = 2,
         bioav = 0,
         rate = 0)
      design[(length(design[,1])+1) : (length(design[,1])+length(t_diff)),] <- tmp[order(tmp$t, -tmp$dose),]
    }
  }
  if(is.null(t_max)) {
    if(length(design$t) > 1) {
      t_max <- utils::tail(design$t,1)
    } else {
       t_max <- utils::tail(design$t,1) + 24 # guess timeframe, user should use tmax argument
    }
    if(!is.null(t_obs) && length(t_obs) > 0) {
      if(is.null(t_max) || is.na(t_max) || t_max < max(t_obs)) { t_max <- max(t_obs) }
    }
    if(!is.null(t_tte) && length(t_tte) > 0) {
      if(is.null(t_tte) || is.na(t_max) || t_max < max(t_tte)) { t_max <- max(t_tte) }
    }
  }
  design <- design[design$t <= t_max,]
  design[length(design[,1])+1,] <- utils::tail(design,1)
  design[length(design[,1]), c("t", "dose")] <- c(t_max,0)

  # now add the covariate values to the design dataset
  if(!is.null(covariates)) {
    for(i in seq(names(covariates))) {
      design[[paste0("cov_", names(covariates)[i])]] <- 0
      design[[paste0("cov_t_", names(covariates)[i])]] <- 0
      design[[paste0("gradients_", names(covariates)[i])]] <- 0
    }
    for(j in seq(names(covariates))) {
      nam <- names(covariates)[j]
      tmp <- covt[covt$name == nam,]
      for(i in 1:length(tmp[,1])) {
        design[design$t >= tmp[i,]$time, c(paste0("cov_", nam))] <- tmp[i,]$value
        design[design$t >= tmp[i,]$time, c(paste0("cov_t_", nam))] <- tmp[i,]$time
        if(tolower(tmp[i,]$implementation) != "locf") {
          if(i < length(tmp[,1])) {
            if((tmp[i+1,]$time - tmp[i,]$time) > 0) {
              design[design$t >= tmp[i,]$time, c(paste0("gradients_", nam))] <- (tmp[i+1,]$value - tmp[i,]$value)  / (tmp[i+1,]$time - tmp[i,]$time)
            }
          } else {
            design[design$t >= tmp[i,]$time, c(paste0("gradients_", nam))] <- 0
          }
        } else {
          design[,c(paste0("gradients_", nam))] <- 0
        }
      }
    }
    # remove covariate points where there is also a dose
    design <- design[!duplicated(paste0(design$t, "_", design$dose, "_", design$dum)),]
    design <- design[!(design$t %in% covt$time & design$t %in% regimen$dose_times & design$dose == 0 & design$dum == 0) | design$t %in% t_obs,]
  }
  design <- design[order(design$t, -design$dum),]
  return(design)
}
