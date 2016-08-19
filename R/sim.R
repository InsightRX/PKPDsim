#' Simulate ODE or analytical equation
#'
#' Simulates a specified regimen using ODE system or analytical equation
#' @param ode function describing the ODE system
#' @param dde function describing the DDE system (not implemented yet)
#' @param parameters model parameters
#' @param omega vector describing the lower-diagonal of the between-subject variability matrix
#' @param omega_type exponential or normal
#' @param n_ind number of individuals to simulate
#' @param regimen a regimen object created using the regimen() function
#' @param adherence List specifying adherence. Simulates adherence using either markov model or binomial sampling.
#' @param A_init vector with the initial state of the ODE system
#' @param covariates list of covariate values to be passed to ODE function
#' @param only_obs only return the observations
#' @param obs_step_size the step size between the observations
#' @param int_step_size the step size for the numerical integrator
#' @param t_max maximum simulation time, if not specified will pick the end of the regimen as maximum
#' @param t_obs vector of observation times, only output these values (only used when t_obs==NULL)
#' @param t_tte vector of observation times for time-to-event simulation
#' @param duplicate_t_obs allow duplicate t_obs in output? E.g. for optimal design calculations when t_obs = c(0,1,2,2,3). Default is FALSE.
#' @param extra_t_obs_bolus include extra t_obs in output for bolus doses? E.g. for a bolus dose at t=24, if FALSE, PKPDsim will output only the trough, so for bolus doses you might want to switch this setting to TRUE. When set to "auto" (default), it will be TRUE by default, but will switch to FALSE whenever `t_obs` is specified manually.
#' @param rtte should repeated events be allowed (FALSE by default)
#' @param covariate_model feature not implemented yet.
#' @param checks perform input checks? Default is TRUE. For calculations where sim_ode is invoked many times (e.g. population estimation, optimal design) it makes sense to switch this to FALSE (after confirming the input is correct) to improve speed.
#' @param verbose show more output
#' @param ... extra parameters
#' @return a data frame of compartments with associated concentrations at requested times
#' @export
#' @seealso \link{sim_ode_shiny}
#' @examples
#' \dontrun{
#'library(ggplot2)
#'library(PKPDsim)
#'p <- list(CL = 38.48,
#'          V  = 7.4,
#'          Q2 = 7.844,
#'          V2 = 5.19,
#'          Q3 = 9.324,
#'          V3 = 111)
#'
#'r1 <- new_regimen(amt = 100,
#'              times = c(0, 24, 36),
#'              type = "infusion")
#'
#'dat <- sim_ode (ode = "pk_3cmt_iv",
#'                par = p,
#'                regimen = r1)
#'
#'ggplot(dat, aes(x=t, y=y)) +
#'  geom_line() +
#'  scale_y_log10() +
#'  facet_wrap(~comp)
#'
#'# repeat with IIV:
#'omega <- c(0.3,       # IIV CL
#'           0.1, 0.3)  # IIV V
#'
#'dat <- sim (ode = "pk_3cmt_iv",
#'                par = p,
#'                omega = omega,
#'                n_ind = 20,
#'                regimen = r1)
#'
#'ggplot(dat, aes(x=t, y=y, colour=factor(id), group=id)) +
#'  geom_line() +
#'  scale_y_log10() +
#'  facet_wrap(~comp)
#'}
sim <- function (ode = NULL,
                 dde = NULL,
                 analytical = NULL,
                 parameters = list(),
                 omega = NULL,
                 omega_type = "exponential",
                 n_ind = 1,
                 regimen = NULL,
                 adherence = NULL,
                 covariates = NULL,
                 covariate_model = NULL,
                 A_init = NULL,
                 only_obs = FALSE,
                 obs_step_size = 1,
                 int_step_size = .1,
                 t_max = NULL,
                 t_obs = NULL,
                 t_tte = NULL,
                 duplicate_t_obs = FALSE,
                 extra_t_obs_bolus = "auto",
                 rtte = FALSE,
                 checks = TRUE,
                 verbose = FALSE,
                 ...
                 ) {
  if(extra_t_obs_bolus == "auto") {
    if(is.null(t_obs)) {
      extra_t_obs_bolus <- TRUE
    } else {
      extra_t_obs_bolus <- FALSE
    }
  }
  if (!is.null(omega)) {
    omega_mat <- triangle_to_full(omega)
    etas   <- MASS::mvrnorm(n = n_ind, mu=rep(0, nrow(omega_mat)), Sigma=omega_mat)
    if( n_ind == 1) {
      etas <- t(matrix(etas))
    }
  }
  if(!is.null(regimen$ss_regimen)) { ## prepend the doses to get to steady state
    regimen_orig <- regimen
    regimen <- join_regimen(regimen_orig$ss_regimen, regimen, interval = regimen_orig$ss_regimen$interval)
  } else {
    regimen_orig <- regimen
  }
  comb <- list()
  p <- as.list(parameters)
  if(is.null(t_obs)) { # find reasonable default to output
    if(is.null(obs_step_size)) {
      if(length(regimen$dose_times) == 1 && regimen$dose_times == 0) {
        obs_step_size <- 1
      } else {
        obs_step_size <- 100
        if(max(regimen$dose_times) < 10000) { obs_step_size <- 100 }
        if(max(regimen$dose_times) < 1000) { obs_step_size <- 10 }
        if(max(regimen$dose_times) < 100) { obs_step_size <- 1 }
        if(max(regimen$dose_times) < 10) { obs_step_size <- .1 }
      }
    }
    if("regimen" %in% class(regimen)) {
      if(length(regimen$dose_times) == 1 && regimen$dose_times == 0) {
        t_obs <- seq(from=regimen$dose_times[1], to=24, by=obs_step_size)
      } else {
        t_obs <- seq(from=0, to=max(regimen$dose_times) + regimen$interval, by=obs_step_size)
      }
    }
  }
  t_obs <- round(t_obs, 8) # make sure the precision is not too high, otherwise NAs will be generated when t_obs specified
  if(is.null(analytical)) {
    if ("character" %in% class(ode)) {
      ode <- get(ode)
    }
  }
  if(checks) {
    if(!is.null(dde)) {
      ode <- dde
    }
    ## test_pointer looks if the model is in memory, will throw error if needs to be recompiled.
    test_pointer(ode)
    if(!is.null(attr(ode, "cpp")) && attr(ode, "cpp")) {
      cpp <- TRUE
    } else {
      cpp <- FALSE
    }
    if(is.null(analytical)) {
      if("function" %in% class(ode) && is.null(attr(ode, "cpp")) || attr(ode, "cpp") == FALSE) {
        stop("Sorry. Non-C++ functions are deprecated.")
      } else {
        if("function" %in% class(ode)) {
          size <- attr(ode,  "size")
        } else {
          size <- attr(get(ode),  "size")
        }
      }
    }
    if(is.null(ode) | is.null(parameters)) {
      stop("Please specify at least the required arguments 'ode' and 'parameters'.")
    }
    if(is.null(regimen)) {
      stop("Please specify a regimen created using the `new_regimen()` function.")
    }
    #  if(!is.null(t_tte)) {
    #    stop("Please specify possible observation times for time-to-event analysis as 't_tte' argument!")
    #  }
    if("function" %in% class(ode) && is.null(attr(ode, "cpp")) || attr(ode, "cpp") == FALSE) {
      stop("Sorry. Non-C++ functions are deprecated.")
    }
    if(!is.null(adherence)) {
      if(adherence$type == "markov") {
        if(!all(c("p01", "p11") %in% names(adherence$markov))) {
          stop("Adherence simulation using Markov model requires specification of p01 and p11!")
        }
      }
    }
    if(!is.null(covariates)) {
      if(class(covariates) != "list") {
        stop("Covariates need to be specified as a list!")
      } else {
        for(key in names(covariates)) {
          if(!"covariate" %in% class(covariates[[key]])) {
            if(class(covariates[[key]]) == "numeric") { # auto-conversion for convenience
              covariates[[key]] <- new_covariate(covariates[[key]])
            }
          }
        }
      }
    }
    ## check parameters specified
    pars_ode <- attr(ode, "parameters")
    rates <- paste0("rate[", 0:(size-1), "]")
    if(!all(pars_ode %in% c(names(parameters), rates))) {
      m <- match(c(names(parameters), names(covariates)), pars_ode)
      if(length(m) == 0) {
        missing <- pars_ode
      } else {
        missing <- pars_ode[-m[!is.na(m)]]
      }
      stop("Not all parameters for this model have been specified. Missing: \n  ", paste(pars_ode[-m[!is.na(m)]], collapse=", "))
    }
    covs_ode <- attr(ode, "covariates")
    if(!is.null(covs_ode)) {
      if(!all(covs_ode %in% c(names(covariates)))) {
        m <- match(names(covariates), covs_ode)
        if(length(m) == 0) {
          missing <- covs_ode
        } else {
          missing <- covs_ode[-m[!is.na(m)]]
        }
        stop("Not all covariates for this model have been specified. Missing: \n  ", paste(missing, collapse=", "))
      }
    }
    if(! any(c("regimen", "regimen_multiple") %in% class(regimen))) {
      stop("Please create a regimen using the new_regimen() function!")
    }
    if(verbose) {
      message("Simulating...")
    }
  } else {
    size <- attr(ode, "size")
  }
  if (!is.null(adherence)) { ## varying regimen due to adherence
    tmp <- list()
    for(i in 1:n_ind) {
      tmp[[i]] <- regimen
    }
    regimen <- tmp
    class(regimen) <- c(class(regimen), "regimen_multiple")
  }
  if("regimen_multiple" %in% class(regimen)) {
    n_ind <- length(regimen)
  } else {
    design <- parse_regimen(regimen, t_max, t_obs, t_tte, p, covariates, ode)
    design_i <- design
    p$dose_times <- regimen$dose_times
    p$dose_amts <- regimen$dose_amts
  }
  if (is.null(A_init)) {
    A_init <- rep(0, size)
  }
  events <- c() # only for tte
  comb <- c()

  for (i in 1:n_ind) {
    p_i <- p
    if("regimen_multiple" %in% class(regimen)) {
      design_i <- parse_regimen(regimen[[i]], t_max, t_obs, t_tte, p_i, covariates)
      if("regimen_multiple" %in% class(regimen)) {
        p_i$dose_times <- regimen[[i]]$dose_times
        p_i$dose_amts <- regimen[[i]]$dose_amts
      }
      if(i == 1 && is.null(t_obs)) { # find reasonable default to output
        if(is.null(obs_step_size)) {
          obs_step_size <- 100
          if(max(design_i$t) < 10000) { obs_step_size <- 100 }
          if(max(design_i$t) < 1000) { obs_step_size <- 10 }
          if(max(design_i$t) < 100) { obs_step_size <- 1 }
          if(max(design_i$t) < 10) { obs_step_size <- .1 }
        }
      }
      t_obs <- seq(from=0, to=max(design_i$t), by=obs_step_size)
    }
    if (!is.null(adherence)) {
      l <- length(design_i[design_i$dose != 0,]$dose)
      if(adherence$type == "markov") {
        adh_i <- new_adherence(n = l,
                               markov = list(p01 = adherence$markov$p01, p11 = adherence$markov$p11))
      } else {
        adh_i <- new_adherence(n = l,
                               p_binom = adherence$p_bionm)
      }
      design_i[design_i$dose != 0,]$dose <- design_i[design_i$dose != 0,]$dose * adh_i
    }
    if (!is.null(omega)) {
      if (omega_type == "exponential") {
        p_i[1:nrow(omega_mat)] <- utils::relist(unlist(utils::as.relistable(p_i[1:nrow(omega_mat)])) * exp(etas[i,]))
      } else {
        p_i[1:nrow(omega_mat)] <- utils::relist(unlist(utils::as.relistable(p_i[1:nrow(omega_mat)])) + etas[i,])
      }
    }
    tmp <- c()
    if(!is.null(t_max)) {
      p_i$dose_times <- p_i$dose_times[p_i$dose_times <= t_max]
    }
    if(length(p_i$t_inf) < length(p_i$dose_times)) {
      p_i$t_inf <- rep(p_i$t_inf[1], length(p_i$dose_times))
    }
    for(k in seq(p_i$dose_times)) {
      design_i[design_i$t >= p_i$dose_times[k] & design_i$t < (p_i$dose_times[k] + p_i$t_inf[k]),]$rate <- (p_i$dose_amts[k] / p_i$t_inf[k])
    }
    p_i$rate <- 0

    #################### Main call to ODE solver / analytical eq solver #######################
    if(!is.null(ode)) {
      tmp <- model(A_init, design_i, p_i, int_step_size)
    } else {
      tmp <- analytical_eqn_wrapper(analytical, design_i, p_i)
    }
    #####################################################################

    des_out <- matrix(unlist(tmp$y), nrow=length(tmp$time), byrow = TRUE)
    dat_ind <- c()
    if(only_obs || !is.null(analytical)) {
      dat_ind <- cbind(id=i, t=tmp$time, comp="obs", y=unlist(tmp$obs))
    } else {
      for (j in 1:length(A_init)) {
        dat_ind <- rbind (dat_ind, cbind(id=i, t=tmp$time, comp=j, y=des_out[,j]))
      }
      dat_ind <- rbind(dat_ind, cbind(id=i, t=tmp$time, comp="obs", y=unlist(tmp$obs)))
    }
    if("regimen_multiple" %in% class(regimen) || !is.null(adherence)) {
      comb <- rbind(comb, dat_ind)
    } else {
      if(i == 1) {
        l_mat <- length(dat_ind[,1])
        comb <- matrix(nrow = l_mat*n_ind, ncol=ncol(dat_ind)) # don't grow but define upfront
      }
      comb[((i-1)*l_mat)+(1:l_mat),] <- dat_ind
    }
  }

  # Add concentration to dataset, and perform scaling and/or transformation:
  comb <- data.frame(comb)
  colnames(comb) <- c("id", "t", "comp", "y")

  # filter out observations
  comb$t <- as.numeric(as.character(comb$t))

  ## following code disactivated for now, no tte functionality
  # if(length(events) > 0) {
  #   events <- data.frame(events)
  #   events <- events[!duplicated(paste0(events$id, "_", events$t)),]
  #   ids <- unique(comb$id)
  #   if(!rtte) { # no repeated TTE
  #     events <- events[!duplicated(events$id),]
  #     ids_ev <- unique(events[,1])
  #   } else {
  #     ids_ev <- unique(events[events[,2] == max(t_tte),1])
  #   }
  #   cens <- setdiff(ids, ids_ev) # censor individuals who didn't have event on last obs day
  #   comb <- rbind(comb, cbind(events, comp="event", y = 1))
  #   if(length(cens)>0) {
  #     comb <- rbind(comb, cbind(id = cens, t = max(comb$t), comp="event", y=0))
  #   }
  # }

  comb <- data.frame(comb)
  comb$id <- as.num(comb$id)
  comb$t  <- as.num(comb$t)
  comb$y  <- as.num(comb$y)
  if(!extra_t_obs_bolus) { ## include the observations at which a bolus dose is added into the output object too
    # comb <- comb %>% dplyr::group_by(id, comp) %>% dplyr::distinct(t)) # we do need to filter out the bolus dose observations
    comb <- comb[!duplicated(paste(comb$id, comb$comp, comb$t, sep="_")),]
  }
  grid <- expand.grid(t_obs, unique(comb$id), unique(comb$comp))
  colnames(grid) <- c("t", "id", "comp")
  suppressMessages({
    comb <- left_join(grid, comb, copy=TRUE)[,c(2,1,3,4)]
  })

  if(!is.null(regimen_orig$ss_regimen)) {
    t_ss <- tail(regimen_orig$ss_regimen$dose_times,1) + regimen_orig$ss_regimen$interval
    comb$t <- as.num(comb$t) - t_ss
    comb <- comb[comb$t >= 0,]
  }

  class(comb) <- c("PKPDsim_data", class(comb))
  attr(comb, "regimen") <- regimen_orig
  attr(comb, "ode_code") <- attr(ode, "code")
  attr(comb, "parameters") <- attr(ode, "parameters")
  return(comb)
}
