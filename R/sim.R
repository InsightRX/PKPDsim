#' Simulate ODE or analytical equation
#'
#' Simulates a specified regimen using ODE system or analytical equation
#' @param ode function describing the ODE system
#' @param analytical analytical equation (function)
#' @param parameters model parameters
#' @param omega vector describing the lower-diagonal of the between-subject variability matrix
#' @param omega_type exponential or normal, specified as vector
#' @param res_var residual variability. Expected a list with arguments `prop`, `add`, and/or `exp`. NULL by default.
#' @param iov_bins allow override of the default IOV bins for a model. Specified as a vector of timepoints specifying the bin separators, e.g. `iov_bins = c(0, 24, 48, 72, 9999)`.
#' @param seed set seed for reproducible results
#' @param sequence if not NULL specifies the pseudo-random sequence to use, e.g. "halton" or "sobol". See `mvrnorm2` for more details.
#' @param n_ind number of individuals to simulate
#' @param regimen a regimen object created using the regimen() function
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
#' @param output_include list specyfing what to include in output table, with keys `parameters` and `covariates`. Both are FALSE by default.
#' @param checks perform input checks? Default is TRUE. For calculations where sim_ode is invoked many times (e.g. population estimation, optimal design) it makes sense to switch this to FALSE (after confirming the input is correct) to improve speed.
#' @param return_design Useful for iterative functions like estimation. Only prepares the design (event table) for the simulation, does not run the actual simulation.
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
                 iov_bins = NULL,
                 seed = NULL,
                 sequence = NULL,
                 n_ind = 1,
                 regimen = NULL,
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
                 return_design = FALSE,
                 output_include = list(parameters = FALSE, covariates = FALSE),
                 ...
                 ) {
  if(!is.null(t_obs)) {
    extra_t_obs <- FALSE # when t_obs specified manually, we want to return the exact requested timepoints without any duplicates
  }
  if(!is.null(seed)) {
    set.seed(seed)
  }
  if(!is.null(regimen$ss_regimen)) { ## prepend the doses to get to steady state
    regimen_orig <- regimen
    regimen <- join_regimen(regimen_orig$ss_regimen, regimen, interval = regimen_orig$ss_regimen$interval)
    t_ss <- max(regimen_orig$ss_regimen$dose_times) + regimen_orig$ss_regimen$interval
    t_obs <- t_obs + t_ss
    if(!is.null(t_max)) t_max <- t_max + t_ss
    ## Also adjust the times for the covariates!
  } else {
    t_ss <- 0
    regimen_orig <- regimen
  }
  if(!is.null(attr(ode, "lagtime")) && attr(ode, "lagtime") != "undefined" && attr(ode, "lagtime") !=
    "NULL") {
    if(class(attr(ode, "lagtime")) %in% c("numeric", "integer")) {
      regimen$dose_times <- regimen$dose_times + attr(ode, "lagtime")
    }
    if(class(attr(ode, "lagtime")) %in% c("character")) {
      regimen$dose_times <- regimen$dose_times + parameters[[attr(ode, "lagtime")]]
    }
  }
  comb <- list()
  p <- as.list(parameters)
  if(!is.null(t_obs)) {
    t_obs <- round(t_obs, 6)
  }
  t_obs_orig <- t_obs
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
    ## Add _kappa parameters (IOV) if not specified by user but required by model
    if(!is.null(attr(ode, "parameters"))) {
      p_mod <- attr(ode, "parameters")
      m <- stringr::str_detect(p_mod, "kappa_")
      if(any(m)) {
        if(verbose) message("Some IOV parameters (kappa) not supplied, setting to 0.")
        p_kappa <- p_mod[m]
        for(key in p_kappa) { # just set to zero, i.e. no IOV
          if(is.null(parameters[[key]])) parameters[[key]] <- 0
        }
        p <- parameters
      }
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
      if(covs_ode != "" && !all(covs_ode %in% c(names(covariates_tmp)))) {
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
  if (!is.null(omega)) {
    if(class(omega) == "matrix") {
      omega_mat <- omega
    }
    if(class(omega) %in% c("numeric", "integer")) {
      omega_mat <- triangle_to_full(omega)
    }
    etas <- mvrnorm2(n = n_ind, mu=rep(0, nrow(omega_mat)), Sigma=omega_mat, sequence=sequence)
    if(n_ind == 1) {
      etas <- t(matrix(etas))
    }
  }
  if("regimen_multiple" %in% class(regimen)) {
    n_ind <- length(regimen)
  } else {
    if(is.null(t_obs)) { # find reasonable default to output
      t_obs <- get_t_obs_from_regimen(
        regimen, obs_step_size, t_max,
        covariates, extra_t_obs)
    }
    if(is.null(covariates_table)) {
      design <- parse_regimen(regimen, t_max, t_obs, t_tte, p, covariates, ode)
    } else {
      design <- parse_regimen(regimen, t_max, t_obs, t_tte, p, covariates[[1]], ode)
    }
    design_i <- design
    p$dose_times <- regimen$dose_times
    p$dose_amts  <- regimen$dose_amts
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
      if(is.null(t_obs)) { # find reasonable default to output
        t_obs <- get_t_obs_from_regimen(
          regimen[[i]], obs_step_size, t_max,
          covariates, extra_t_obs)
      }
      design_i <- parse_regimen(regimen[[i]], t_max, t_obs, t_tte, p_i, covariates_tmp)
      if("regimen_multiple" %in% class(regimen)) {
        p_i$dose_times <- regimen[[i]]$dose_times
        p_i$dose_amts <- regimen[[i]]$dose_amts
      }
    }
    t_obs <- round(t_obs, 6) # make sure the precision is not too high, otherwise NAs will be generated when t_obs specified
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

    ## Inter-occasion variability:
    if(is.null(iov_bins)) {
      iov_bins <- c(0, 99999) # dummy
    }
    if(return_design) {
      return(list(
        A_init = A_init,
        design = design_i,
        p = p_i,
        int_step_size = int_step_size,
        t_obs = t_obs,
        t_obs_orig = t_obs_orig,
        iov_bins = iov_bins
      ))
    }

    #################### Main call to ODE solver / analytical eq solver #######################
    if(!is.null(ode)) {
      tmp <- ode(A_init, design_i, p_i, iov_bins, int_step_size)
    } else {
      tmp <- analytical_eqn_wrapper(analytical, design_i, p_i)
    }
    #####################################################################

    des_out <- matrix(unlist(tmp$y), nrow=length(tmp$time), byrow = TRUE)
    dat_ind <- c()
    obs <- attr(ode, "obs")
    dat_obs <- c()
    if(!is.null(obs) && length(obs$cmt) > 1) {
      for(j in 1:length(obs$cmt)) {
        lab <- ifelse(!is.null(obs$label[j]), obs$label[j], paste0("obs",j))
        dat_obs <- rbind(dat_obs, cbind(id=i, t=tmp$time, comp=lab, y=unlist(tmp[[paste0("obs",j)]])))
      }
    } else {
      dat_obs <- cbind(id=i, t=tmp$time, comp="obs", y=unlist(tmp$obs))
    }
    if(only_obs || !is.null(analytical)) {
      dat_ind <- dat_obs
    } else {
      for (j in 1:length(A_init)) {
        dat_ind <- rbind(dat_ind, cbind(id=i, t=tmp$time, comp=j, y=des_out[,j]))
      }
      dat_ind <- rbind(dat_ind, dat_obs)
    }

    ## Add parameters, variables and/or covariates, if needed. Implementation is slow, can be improved.
    if(!is.null(output_include$parameters) && output_include$parameters) {
      dat_ind <- as.matrix(merge(dat_ind, p_i[!names(p_i) %in% c("dose_times", "dose_amts", "rate")]))
    }
    cov_names <- NULL
    if(!is.null(output_include$covariates) && output_include$covariates && !is.null(covariates)) {
      cov_names <- names(covariates)
      for(key in cov_names) {
        dat_ind <- cbind(dat_ind, design_i[[paste0("cov_", key)]] + (design_i$t - design_i[[paste0("cov_t_", key)]]) * design_i[[paste0("gradients_", key)]] )
      }
    }
    var_names <- NULL
    if(!is.null(output_include$variables) && output_include$variables && !is.null(attr(ode, "variables"))) {
      var_names <- attr(ode, "variables")
      if(!is.null(cov_names)) {
        var_names <- var_names[!var_names %in% cov_names]
      }
      var_names <- var_names[var_names != "NULL"]
    }
    if(!is.null(var_names) && length(var_names) > 0) {
      for(key in var_names) {
        dat_ind <- cbind(dat_ind, tmp[[key]])
      }
    }

    if("regimen_multiple" %in% class(regimen) || !is.null(covariates_table)) {
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
  all_names <- unique(c(par_names, cov_names, var_names))
  colnames(comb) <- c("id", "t", "comp", "y", all_names)
  col_names <- c("id", "t", "y", all_names)
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
    if(!is.null(all_names) && length(all_names) > 0) {
      comb <- dplyr::left_join(grid, comb, copy=TRUE)[, c("id", "t", "comp", "y", all_names)]
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
