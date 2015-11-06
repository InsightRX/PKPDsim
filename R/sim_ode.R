#' Simulate ODE
#'
#' Simulates a specified ODE system and regimen
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
#' @param duplicate_t_obs allow duplicate t_obs in output? E.g. for a bolus dose at t=24, the default (FALSE) will be to output only the trough, so for bolus doses you might want to switch this setting to TRUE (when used for plotting).
#' @param rtte should repeated events be allowed (FALSE by default)
#' @param covariate_model feature not implemented yet.
#' @param verbose show more output
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
#'dat <- sim_ode (ode = "pk_3cmt_iv",
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
sim_ode <- function (ode = NULL,
                     dde = NULL,
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
                     rtte = FALSE,
                     verbose = FALSE
                     ) {
#   if (!is.null(covariate_model) && !is.null(covariates)) {
#     n_ind <- length(t(covariates[,1]))
#     message(paste0("Simulating ", n_ind, " individuals with covariates...\n"))
#     if (sum("tbl_df" %in% class(covariates))==0) {
#       covariates <- data_frame(covariates)
#     }
#   }
#   if ((!is.null(covariate_model) && is.null(covariates)) | (is.null(covariate_model) && !is.null(covariates))) {
#     stop("For models with covariates, specify both the 'covariate_model' and the 'covariates' arguments. See help for more information.")
#   }
  if (!is.null(dde)) {
    lsoda_func <- deSolve::dede
  } else {
    lsoda_func <- deSolve::lsoda
  }
  if(!is.null(dde)) {
    ode <- dde
  }
  if ("character" %in% class(ode)) {
    ode <- get(ode)
  }# else {
  #  stop("Error: the 'ode' argument to this function should be a character string referencing the function, not the function itself.")
  #}
  test_pointer(ode)
  if(!is.null(attr(ode, "cpp")) && attr(ode, "cpp")) {
    cpp <- TRUE
  } else {
    cpp <- FALSE
  }
  if(is.null(ode) | is.null(parameters)) {
    stop("Please specify at least the required arguments 'ode' and 'parameters'.")
  }
  if(is.null(regimen)) {
    regimen <- new_regimen()
  }
#   if(!is.null(t_tte)) {
#     stop("Please specify possible observation times for time-to-event analysis as 't_tte' argument!")
#   }
  if("function" %in% class(ode) && is.null(attr(ode, "cpp")) || attr(ode, "cpp") == FALSE) {
    stop("Sorry. Non-C++ functions are deprecated.")
  } else {
    if("function" %in% class(ode)) {
      size <- attr(ode,  "size")
    } else {
      size <- attr(get(ode),  "size")
    }
  }
  if (!is.null(omega)) {
    omega_mat <- triangle_to_full(omega)
    etas   <- MASS::mvrnorm(n = n_ind, mu=rep(0, nrow(omega_mat)), Sigma=omega_mat)
    if(n_ind == 1) {
      etas <- t(matrix(etas))
    }
  }
  if(!is.null(adherence)) {
    if(adherence$type == "markov") {
      if(!all(c("p01", "p11") %in% names(adherence$markov))) {
        stop("Adherence simulation using Markov model requires specification of p01 and p11!")
      }
    }
  }
  comb <- list()
  p <- parameters
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
        t_obs <- seq(from=0, to=max(regimen$dose_times)*1.2, by=obs_step_size)
      }
    }
  }
  if(! any(c("regimen", "regimen_multiple") %in% class(regimen))) {
    stop("Please create a regimen using the new_regimen() function!")
  }
  if("regimen_multiple" %in% class(regimen)) {
    n_ind <- length(regimen)
  } else {
    design <- parse_regimen(regimen, t_max, t_obs, t_tte, p, covariates)
    design_i <- design
    p$dose_times <- regimen$dose_times
    p$dose_amts <- regimen$dose_amts
  }
  if (is.null(A_init)) {
    A_init <- rep(0, size)
  }
  events <- c() # only for tte
  comb <- c()
  if(cpp) { # check parameters specified
    pars_ode <- attr(ode, "parameters")
    if(!all(pars_ode %in% c(names(parameters), names(covariates)))) {
      m <- match(c(names(parameters), names(covariates)), pars_ode)
      stop("Not all parameters for this model have been specified. Missing parameters are: \n  ", paste(pars_ode[-m[!is.na(m)]], collapse=", "))
    }
  }
  if(verbose) {
    message("Simulating...")
  }
  for (i in 1:n_ind) {
    p_i <- p
    if("regimen_multiple" %in% class(regimen)) {
      design_i <- parse_regimen(regimen[[i]], t_max, t_obs, t_tte, p_i, covariates)
      if(regimen[[i]]$type == "infusion") {
        p_i$t_inf <- regimen$t_inf
        p_i$dose_type <- "infusion"
      } else {
        p_i$dose_type <- "bolus"
      }
      p_i$dose_times <- regimen[[i]]$dose_times
      p_i$dose_amts <- regimen[[i]]$dose_amts
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
    } else {
      if(regimen$type == "infusion") {
        p_i$t_inf <- regimen$t_inf
        p_i$dose_type <- "infusion"
      } else {
        p_i$dose_type <- "bolus"
      }
    }
    times <- unique(c(seq(from=0, to=tail(design_i$t,1), by=int_step_size), t_obs))
    A_init_i <- A_init
    if (!is.null(adherence)) {
      if(adherence$type == "markov") {
        adh_i <- new_adherence(n = length(design_i[design_i$dum == 0,]$dose),
                               markov = list(p01 = adherence$markov$p01, p11 = adherence$markov$p11))
      } else {
        adh_i <- new_adherence(n = length(design_i[design_i$dum == 0,]$dose),
                               p_binom = adherence$p_bionm)
      }
      design_i[design_i$dum == 0,]$dose <- design_i[design_i$dum == 0,]$dose * adh_i
    }
    if (!is.null(omega)) {
      if (omega_type=="exponential") {
        p_i[1:nrow(omega_mat)] <- relist(unlist(as.relistable(p_i[1:nrow(omega_mat)])) * exp(etas[i,]))
      } else {
        p_i[1:nrow(omega_mat)] <- relist(unlist(as.relistable(p_i[1:nrow(omega_mat)])) + etas[i,])
      }
    }
    event_occurred <- FALSE
    tmp <- c()
    prv_cumhaz <- 0
    if(cpp) {
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
      tmp <- ode (A_init_i, design_i, p_i, int_step_size)
      des_out <- cbind(matrix(unlist(tmp$y), nrow=length(tmp$time), byrow = TRUE))
      dat_ind <- c()
      for (j in 1:length(A_init_i)) {
        dat_ind <- rbind (dat_ind, cbind(id=i, t=tmp$time, comp=j, y=des_out[,j]))
      }
      if(only_obs) {
        dat_ind <- cbind(id=i, t=tmp$time, comp="obs", y=unlist(tmp$obs))
      } else {
        dat_ind <- rbind(dat_ind, cbind(id=i, t=tmp$time, comp="obs", y=unlist(tmp$obs)))
      }
      if(i == 1) {
        l_mat <- length(dat_ind[,1])
        comb <- matrix(nrow = l_mat*n_ind, ncol=ncol(dat_ind)) # don't grow but define upfront
      }
      if("regimen_multiple" %in% class(regimen)) {
        comb <- rbind(comb, dat_ind)
      } else {
        comb[((i-1)*l_mat)+(1:l_mat),] <- dat_ind
      }
#      comb <- data.frame(dat_ind)
    } else {
      if(class(A_init) == "function") {
        A_init_i = A_init(p_i)
      }
      for (k in 1:(length(design_i$t)-1)) {
        if (k > 1) {
          A_upd <- dat[dat$comp!="cumhaz" & dat$t==tail(time_window,1),][,]$y
          if(event_occurred) {
            A_upd[attr(ode, "cumhaz")[["cmt"]]] <- 0 # reset cumulative hazard
          }
        } else {
          A_upd <- A_init_i
        }
        p_i$rate <- 0
        if(p_i$dose_type != "infusion") {
          A_upd[regimen$cmt] <- A_upd[regimen$cmt] + design_i[design_i$dum == 0,]$dose[k]
        } else {
          if(design_i$dose[k] > 0) {
            p_i$rate <- design_i$dose[k] / p_i$t_inf
          }
        }
        time_window <- times[(times >= design_i$t[k]) & (times <= design_i$t[k+1])]
        dat <- cbind(id = i, num_int_wrapper (time_window, A_upd, ode, p_i, lsoda_func))
        if(!is.null(attr(ode, "cumhaz"))) {
          event_occurred <- FALSE
          dat <- rbind (dat, dat %>%
                          dplyr::filter(comp == attr(ode, "cumhaz")[["cmt"]]) %>%
                          dplyr::mutate(comp = "cumhaz", y = y-prv_cumhaz))
          tmp <- dat %>% dplyr::filter(comp == "cumhaz") %>% dplyr::filter(t %in% t_tte) %>% tail(1)
          if(length(tmp[,1])>0) {
            prv_cumhaz <- tmp$y
            if(runif(1) > cumhaz_to_surv(tmp$y)) {
              events <- rbind(events, cbind(id = i, t = tmp$t[1]))
              event_occurred <- TRUE
            } else {
              event_occurred <- FALSE
            }
          }
        }
        comb <- rbind(comb, dat)
      }
    }
  }

  # Add concentration to dataset, and perform scaling and/or transformation:
  comb <- data.frame(comb)
  colnames(comb) <- c("id", "t", "comp", "y")
  if(!cpp) { # For simulations with Cpp code this part is already done, for deSolve this still needs to be done.
      if(!is.null(attr(ode, "obs"))) {
        scale <- rep(1, length(attr(ode, "obs")[["scale"]]))
        if(!is.null(attr(ode, "obs")[["labels"]])) {
          labels <- attr(ode, "obs")[["labels"]]
        } else {
          if (length(scale) == 1) {
            labels <- "obs"
          } else {
            labels <- paste0("obs_", attr(ode, "obs")[["labels"]],
                             seq(from=1, to=length(attr(ode, "obs")[["scale"]])))
          }
        }
        if (!is.null(attr(ode, "obs")[["scale"]])) {
          suppressWarnings({
            for (i in seq(attr(ode, "obs")[["scale"]])) {
              if(!is.na(as.numeric(attr(ode, "obs")[["scale"]][i]))) {
                scale[i] <- as.numeric(attr(ode, "obs")[["scale"]][i])
              } else {
                scale[i] <- as.numeric(p[[attr(ode, "obs")[["scale"]][i]]])
              }
            }
          })
        }
        for (i in seq(labels)) {
          comb <- rbind (comb, comb %>% dplyr::filter(comp == attr(ode, "obs")[["cmt"]][i]) %>% dplyr::mutate(comp = labels[i], y = y/scale[i]))
          if(!is.null(attr(ode, "obs")[["trans"]][i])) {
            if(class(attr(ode, "obs")[["trans"]][i]) == "character") {
              trans_func <- get(attr(ode, "obs")[["trans"]][i])
              comb[comb$comp == labels[i],]$y <- trans_func(comb[comb$comp == labels[i],]$y)
            }
          }
        }
      }
      # comb <- comb %>% dplyr::filter(comp %in% labels)
  }
  # filter out observations
  comb$t <- as.numeric(as.character(comb$t))
  if(!is.null(t_obs)) {
    pick_closest_vec <- function(x, vec) { # pick closests time points. If _times integrator is used this is not necessary, but leaving in just to make sure
      pick_closest <- function(x) {
        which(abs(vec-x) == min(abs(vec-x)))
      }
      unlist(lapply(x, pick_closest))
    }
    idx <- pick_closest_vec(t_obs, comb$t)
    comb <- comb[idx,]
  }
  if(length(events)>0) {
    events <- data.frame(events)
    events <- events[!duplicated(paste0(events$id, "_", events$t)),]
    ids <- unique(comb$id)
    if(!rtte) { # no repeated TTE
      events <- events[!duplicated(events$id),]
      ids_ev <- unique(events[,1])
    } else {
      ids_ev <- unique(events[events[,2] == max(t_tte),1])
    }
    cens <- setdiff(ids, ids_ev) # censor individuals who didn't have event on last obs day
    comb <- rbind(comb, cbind(events, comp="event", y = 1))
    if(length(cens)>0) {
      comb <- rbind(comb, cbind(id = cens, t = max(comb$t), comp="event", y=0))
    }
  }
  comb <- data.frame(comb)
  as.num <- function(x) {as.numeric(as.character(x))}
  comb$id <- as.num(comb$id)
  comb$t <- as.num(comb$t)
  comb$y <- as.num(comb$y)
  comb <- data.frame(comb %>% arrange(id, comp, t))
  if(!duplicate_t_obs) {
    comb <- data.frame(comb %>% dplyr::group_by(id, comp) %>% dplyr::distinct(t))
  }
  class(comb) <- c(class(comb), "PKPDsim")
  return(comb)
}
