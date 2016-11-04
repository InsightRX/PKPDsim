#' Simulate ODE or analytical equation
#'
#' Simulates a specified regimen using ODE system or analytical equation
#' @param ode function describing the ODE system
#' @param analytical analytical equation (function)
#' @param parameters model parameters
#' @param omega vector describing the lower-diagonal of the between-subject variability matrix
#' @param omega_type exponential or normal, specified as vector
#' @param res_var residual variability. Expected a list with arguments `prop`, `add`, and/or `exp`. NULL by default.
#' @param sequence for simulations, if not NULL specifies the pseudo-random sequence to use, e.g. "halton" or "sobol". See `mvrnorm2` for more details.
#' @param n_ind number of individuals to simulate
#' @param regimen a regimen object created using the regimen() function
#' @param adherence List specifying adherence. Simulates adherence using either markov model or binomial sampling.
#' @param A_init vector with the initial state of the ODE system
#' @param covariates list of covariates (for single individual) created using `new_covariate()` function
#' @param covariates_table data.frame (or unnamed list of named lists per individual) with covariate values
#' @param covariates_implementation used only for `covariates_table`, a named list of covariate implementation methods per covariate, e.g. `list(WT = "interpolate", BIN = "locf")`
#' @param only_obs only return the observations
#' @param obs_step_size the step size between the observations
#' @param int_step_size the step size for the numerical integrator
#' @param t_max maximum simulation time, if not specified will pick the end of the regimen as maximum
#' @param t_obs vector of observation times, only output these values (only used when t_obs==NULL)
#' @param t_tte vector of observation times for time-to-event simulation
#' @param duplicate_t_obs allow duplicate t_obs in output? E.g. for optimal design calculations when t_obs = c(0,1,2,2,3). Default is FALSE.
#' @param extra_t_obs include extra t_obs in output for bolus doses? This is only activated when `t_obs` is not specified manually. E.g. for a bolus dose at t=24, if FALSE, PKPDsim will output only the trough, so for bolus doses you might want to switch this setting to TRUE. When set to "auto" (default), it will be TRUE by default, but will switch to FALSE whenever `t_obs` is specified manually.
#' @param rtte should repeated events be allowed (FALSE by default)
#' @param covariate_model feature not implemented yet.
#' @param output_include list specyfing what to include in output table, with keys `parameters` and `covariates`. Both are FALSE by default.
#' @param checks perform input checks? Default is TRUE. For calculations where sim_ode is invoked many times (e.g. population estimation, optimal design) it makes sense to switch this to FALSE (after confirming the input is correct) to improve speed.
#' @param as_date output time as datetime? FALSE by default
#' @param base_date base date to use when using as_date. Will use current time if NULL (default)
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
                 analytical = NULL,
                 parameters = list(),
                 omega = NULL,
                 omega_type = "exponential",
                 res_var = NULL,
                 sequence = NULL,
                 n_ind = 1,
                 regimen = NULL,
                 adherence = NULL,
                 covariates = NULL,
                 covariates_table = NULL,
                 covariates_implementation = list(),
                 A_init = NULL,
                 only_obs = FALSE,
                 obs_step_size = 1,
                 int_step_size = .1,
                 t_max = NULL,
                 t_obs = NULL,
                 t_tte = NULL,
                 duplicate_t_obs = FALSE,
                 extra_t_obs = TRUE,
                 rtte = FALSE,
                 checks = TRUE,
                 verbose = FALSE,
                 output_include = list(parameters = FALSE, covariates = FALSE),
                 ...
                 ) {
  if(!is.null(t_obs)) {
    extra_t_obs <- FALSE # when t_obs specified manually, we want to return the exact requested timepoints without any duplicates
  }
  if (!is.null(omega)) {
    if(class(omega) == "matrix") {
      omega_mat <- omega
    }
    if(class(omega) %in% c("numeric", "integer")) {
      omega_mat <- triangle_to_full(omega)
    }
    etas   <- mvrnorm2(n = n_ind, mu=rep(0, nrow(omega_mat)), Sigma=omega_mat, sequence=sequence)
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
  if(!is.null(attr(ode, "lagtime")) && attr(ode, "lagtime") != "undefined") {
    if(class(attr(ode, "lagtime")) %in% c("numeric", "integer")) {
      regimen$dose_times <- regimen$dose_times + attr(ode, "lagtime")
    }
    if(class(attr(ode, "lagtime")) %in% c("character")) {
      regimen$dose_times <- regimen$dose_times + parameters[[attr(ode, "lagtime")]]
    }
  }
  comb <- list()
  p <- as.list(parameters)
  t_obs_orig <- t_obs
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
      if(!is.null(t_max)) {
        t_obs <- seq(from=0, to=t_max, by=obs_step_size)
      } else {
        if(length(regimen$dose_times) == 1 && regimen$dose_times == 0) {
          t_obs <- seq(from=regimen$dose_times[1], to=24, by=obs_step_size)
        } else {
          t_obs <- seq(from=0, to=max(regimen$dose_times) + regimen$interval, by=obs_step_size)
        }
      }
    }
    ## add timepoints at which covariate is changing to t_obs:
    if(extra_t_obs) {
      func <- function(x) { return(x$times) }
      if(!is.null(covariates)) {
        t_obs <- unique(c(t_obs, unique(unlist(lapply(covariates, func )))))
        t_obs <- t_obs[order(t_obs)]
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
    ## test_pointer looks if the model is in memory, will throw error if needs to be recompiled.
    test_pointer(ode)
    if(is.null(ode) && is.null(analytical)) {
      stop("Either an ODE system (`ode`) or an analytical equation (`analytical`) should be provided to `sim()`")
    }
    if(!is.null(attr(ode, "cpp")) && attr(ode, "cpp")) {
      cpp <- TRUE
    } else {
      cpp <- FALSE
    }
    if(is.null(analytical)) {
      if("function" %in% class(ode) && is.null(attr(ode, "cpp")) || attr(ode, "cpp") == FALSE) {
        stop("Sorry. Non-C++ functions are deprecated as input for ODE.")
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
      if(!is.null(covariates_table)) {
        stop("Both `covariates and `covariates_table` are specified!")
      }
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
    if(!is.null(covariates_table)) {
      if(class(covariates_table) %in% c("data.frame", "data.table")) {
        covariates_table <- covariates_table_to_list(covariates_table, covariates_implementation)
      }
      if(class(covariates_table) != "list") {
        stop("Sorry, covariates for population seem to be misspecified. See manual for more information.")
      }
      if(length(covariates_table) != n_ind) {
        n_ind <- length(covariates_table)
      }
      message(paste0("Simulating ", n_ind, " individuals from covariate definitions."))
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
      if(!is.null(covariates_table)) {
        covariates_tmp <- covariates_table[[1]]
      } else {
        covariates_tmp <- covariates
      }
      if(!all(covs_ode %in% c(names(covariates_tmp)))) {
        m <- match(names(covariates_tmp), covs_ode)
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
    if(is.null(covariates_table)) {
      design <- parse_regimen(regimen, t_max, t_obs, t_tte, p, covariates, ode)
    } else {
      design <- parse_regimen(regimen, t_max, t_obs, t_tte, p, covariates[[1]], ode)
    }
    design_i <- design
    p$dose_times <- regimen$dose_times
    p$dose_amts <- regimen$dose_amts
  }
  if (is.null(A_init)) {
    A_init <- rep(0, size)
  }
  events <- c() # only for tte
  comb <- c()
  if("regimen_multiple" %in% class(regimen) && !is.null(covariates_table)) {
    stop("Sorry, can't simulate multiple regimens for a population in single call to PKPDsim. Use a loop instead.")
  }
  for (i in 1:n_ind) {
    p_i <- p
    if(!is.null(covariates_table)) {
      covariates_tmp <- covariates_table[[1]]
      design_i <- parse_regimen(regimen, t_max, t_obs, t_tte, p_i, covariates_table[[i]])
    } else {
      covariates_tmp <- covariates
    }
    if("regimen_multiple" %in% class(regimen)) {
      design_i <- parse_regimen(regimen[[i]], t_max, t_obs, t_tte, p_i, covariates_tmp)
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
        t_obs <- seq(from=0, to=max(design_i$t), by=obs_step_size)
      }
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
      tmp <- ode(A_init, design_i, p_i, int_step_size)
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

    ## Add parameters and covariates, if needed. Implementation is slow, can be improved.
    if(!is.null(output_include$parameters) && output_include$parameters) {
      dat_ind <- as.matrix(merge(dat_ind, p_i[!names(p_i) %in% c("dose_times", "dose_amts", "rate")]))
    }

    if(!is.null(output_include$covariates) && output_include$covariates) {
      dat_ind <- as.matrix(merge(dat_ind, design_i[1,paste0("cov_", names(covariates_tmp))]))
      for(key in names(covariates_tmp)) {
        if(length(covariates_tmp[[key]]$value) > 1) { # timevarying covariates
          for(s in 2:length(covariates_tmp[[key]]$times)) {
            dat_ind[as.num(dat_ind[,2]) >= covariates_tmp[[key]]$times[s], paste0("cov_",key)] <- covariates_tmp[[key]]$value[s]
          }
        }
      }
    }

    if("regimen_multiple" %in% class(regimen) || !is.null(adherence) || !is.null(covariates_table)) {
      comb <- data.table::rbindlist(list(comb, data.table::as.data.table(dat_ind)))
    } else {
      if(i == 1) { ## faster way: data.frame with prespecified length
        l_mat <- length(dat_ind[,1])
        comb <- matrix(nrow = l_mat*n_ind, ncol=ncol(dat_ind)) # don't grow but define upfront
      }
      comb[((i-1)*l_mat)+(1:l_mat),1:length(dat_ind[1,])] <- dat_ind
    }
  }

  # Add concentration to dataset, and perform scaling and/or transformation:
  comb <- data.frame(comb)
  par_names <- NULL
  if(!is.null(output_include$parameters) && output_include$parameters) {
    par_names <- names(p_i)[!names(p_i) %in% c("dose_times", "dose_amts", "rate")]
  }
  cov_names <- NULL
  if(!is.null(output_include$covariates) && output_include$covariates) {
    cov_names <- names(covariates_tmp)
  }
  colnames(comb) <- c("id", "t", "comp", "y", par_names, cov_names)
  col_names <- c("id", "t", "y", par_names, cov_names)
  for(key in col_names) {
    comb[[key]] <- as.num(comb[[key]])
  }
  if(!extra_t_obs) {
    ## include the observations at which a bolus dose is added into the output object too
    comb <- comb[!duplicated(paste(comb$id, comb$comp, comb$t, sep="_")),]
  } else { # only remove duplicates at t=0
    comb <- comb[!(duplicated(paste(comb$id, comb$comp, comb$t, sep="_")) & comb$t == 0),]
  }
  grid <- expand.grid(t_obs, unique(comb$id), unique(comb$comp))
  colnames(grid) <- c("t", "id", "comp")
  suppressWarnings(suppressMessages( ## left join is a bit too chatty
    if(!is.null(par_names) || !is.null(cov_names)) {
      comb <- dplyr::left_join(grid, comb, copy=TRUE)[, c("id", "t", "comp", "y", par_names, cov_names)]
    } else {
      comb <- dplyr::left_join(grid, comb, copy=TRUE)[, c("id", "t", "comp", "y")]
    }
  ))

  if(!is.null(regimen_orig$ss_regimen)) {
    t_ss <- utils::tail(regimen_orig$ss_regimen$dose_times,1) + regimen_orig$ss_regimen$interval
    comb$t <- as.num(comb$t) - t_ss
    comb <- comb[comb$t >= 0,]
  }

  ## add residual variability
  if(!is.null(res_var)) {
    comb[comb$comp == 'obs',]$y <- add_ruv(comb[comb$comp == 'obs',]$y, res_var)
  }

  class(comb) <- c("PKPDsim_data", class(comb))
  attr(comb, "regimen") <- regimen_orig
  attr(comb, "ode_code") <- attr(ode, "code")
  attr(comb, "parameters") <- attr(ode, "parameters")
  return(comb)
}
