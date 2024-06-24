#' Simulate ODE or analytical equation
#'
#' Simulates a specified regimen using ODE system or analytical equation
#'
#' @param ode function describing the ODE system
#' @param analytical string specifying analytical equation model to use (similar to ADVAN1-5 in NONMEM). If specified, will not use ODEs.
#' @param parameters model parameters
#' @param parameters_table dataframe of parameters (with parameters as columns) containing parameter estimates for individuals to simulate. Formats accepted: data.frame, data.table, or list of lists.
#' @param mixture_group mixture group for models containing mixtures. Should be either `1` or `2`, since only two groups are currently allowed.
#' @param omega vector describing the lower-diagonal of the between-subject variability matrix
#' @param omega_type exponential or normal, specified as vector
#' @param res_var residual variability. Expected a list with arguments `prop`, `add`, and/or `exp`. NULL by default.
#' @param iov_bins allow override of the default IOV bins for a model. Specified as a vector of timepoints specifying the bin separators, e.g. `iov_bins = c(0, 24, 48, 72, 9999)`.
#' @param seed set seed for reproducible results
#' @param sequence if not NULL specifies the pseudo-random sequence to use, e.g. "halton" or "sobol". See `mvrnorm2` for more details.
#' @param n_ind number of individuals to simulate
#' @param event_table use a previously created `design` object used for ODE simulation instead of calling create_event_table() to create a new one. Especially useful for repeated calling of sim(), such as in optimizations or optimal design analysis. Also see `sim_core()` for even faster simulations using precalculated `design` objects.
#' @param regimen a regimen object created using the regimen() function
#' @param lagtime either a value (numeric) or a parameter (character) or NULL.
#' @param A_init vector with the initial state of the ODE system
#' @param covariates list of covariates (for single individual) created using `new_covariate()` function
#' @param covariates_table data.frame (or unnamed list of named lists per individual) with covariate values
#' @param covariates_implementation used only for `covariates_table`, a named list of covariate implementation methods per covariate, e.g. `list(WT = "interpolate", BIN = "locf")`
#' @param covariate_model R code used to pre-calculate effective parameters for use in ADVAN-style analytical equations. Not used in ODE simulations.
#' @param only_obs only return the observations
#' @param obs_step_size the step size between the observations
#' @param int_step_size the step size for the numerical integrator
#' @param t_max maximum simulation time, if not specified will pick the end of the regimen as maximum
#' @param t_obs vector of observation times, only output these values (only used when t_obs==NULL)
#' @param t_tte vector of observation times for time-to-event simulation
#' @param t_init initialization time before first dose, default 0.
#' @param obs_type vector of observation types. Only valid in combination with equal length vector `t_obs`.
#' @param duplicate_t_obs allow duplicate t_obs in output? E.g. for optimal design calculations when t_obs = c(0,1,2,2,3). Default is FALSE.
#' @param extra_t_obs include extra t_obs in output for bolus doses? This is only activated when `t_obs` is not specified manually. E.g. for a bolus dose at t=24, if FALSE, PKPDsim will output only the trough, so for bolus doses you might want to switch this setting to TRUE. When set to "auto" (default), it will be TRUE by default, but will switch to FALSE whenever `t_obs` is specified manually.
#' @param rtte should repeated events be allowed (FALSE by default)
#' @param output_include list specifying what to include in output table, with keys `parameters` and `covariates`. Both are FALSE by default.
#' @param checks perform input checks? Default is TRUE. For calculations where sim_ode is invoked many times (e.g. population estimation, optimal design) it makes sense to switch this to FALSE (after confirming the input is correct) to improve speed.
#' @param return_event_table return the event table for the simulation only, does not run the actual simulation. Useful for iterative use of sim().
#' @param return_design returns the design (event table and several other details) for the simulation, does not run the actual simulation. Useful for iterative functions like estimation in combination with `sim_core()`, e.g. for estimation and optimal design.
#' @param verbose show more output
#' @param ... extra parameters
#' @return a data frame of compartments with associated concentrations at requested times
#' @export
#' @seealso \link{sim_ode_shiny}
#' @return Simulated regimen
#' @examples
#' \donttest{
#' p <- list(
#'   CL = 38.48,
#'   V  = 7.4,
#'   Q  = 7.844,
#'   V2 = 5.19,
#'   Q2  = 9.324,
#'   V3 = 111
#' )
#'
#' omega <- c(0.3,       # IIV CL
#'            0.1, 0.3)  # IIV V
#'
#' r1 <- new_regimen(
#'   amt = 100,
#'   times = c(0, 24, 36),
#'   type = "infusion"
#' )
#'
#' mod <- new_ode_model("pk_3cmt_iv")
#' dat <- sim(
#'   ode = mod,
#'   parameters = p,
#'   omega = omega,
#'   n_ind = 20,
#'   regimen = r1
#' )
#' }
sim <- function (ode = NULL,
                 analytical = NULL,
                 parameters = NULL,
                 parameters_table = NULL,
                 mixture_group = NULL,
                 omega = NULL,
                 omega_type = "exponential",
                 res_var = NULL,
                 iov_bins = NULL,
                 seed = NULL,
                 sequence = NULL,
                 n_ind = 1,
                 event_table = NULL,
                 regimen = NULL,
                 lagtime = NULL,
                 covariates = NULL,
                 covariates_table = NULL,
                 covariates_implementation = list(),
                 covariate_model = NULL,
                 A_init = NULL,
                 only_obs = FALSE,
                 obs_step_size = NULL,
                 int_step_size = 0.01,
                 t_max = NULL,
                 t_obs = NULL,
                 t_tte = NULL,
                 t_init = 0,
                 obs_type = NULL,
                 duplicate_t_obs = FALSE,
                 extra_t_obs = TRUE,
                 rtte = FALSE,
                 checks = TRUE,
                 verbose = FALSE,
                 return_event_table = FALSE,
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
  ## Add duplicate "doses" to regimen, e.g. for double-absorption compartments
  dose_dupl <- attr(ode, "dose")$duplicate
  if(!is.null(dose_dupl)) {
    if(is.null(regimen$cmt)) {
      regimen$cmt <- attr(ode, "dose")$cmt
    }
    regimen_dupl <- regimen
    for(i in 1:length(dose_dupl)) {
      regimen_dupl$cmt <- dose_dupl[i]
      regimen <- merge_regimen(list(regimen, regimen_dupl))
    }
  }
  if(!is.null(attr(ode, "lagtime")) && attr(ode, "lagtime")[1] != "undefined" && attr(ode, "lagtime")[1] != "NULL") {
    if(is.null(lagtime)) { # only override from metadata if not specified by user
      lagtime <- attr(ode, "lagtime")
    }
  }
  if(!is.null(lagtime)) {
    regimen <- apply_lagtime(regimen, lagtime, parameters, attr(ode, "cmt_mapping"))
  }
  if(!is.null(attr(ode, "dose")$duration_scale)) {
    regimen <- apply_duration_scale(
      regimen,
      attr(ode, "dose")$duration_scale,
      parameters,
      attr(ode, "cmt_mapping")
    )
  }
  if(t_init != 0) regimen$dose_times <- regimen$dose_times + t_init
  p <- as.list(parameters)
  if(!is.null(t_obs)) {
    t_obs <- round(t_obs, 6)
  }
  t_obs_orig <- t_obs + t_init
  if(checks) {
    if(!is.null(parameters) && !is.null(parameters_table)) {
      stop("Both `parameters` and `parameters_table` are specified!")
    }
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
    if(!is.null(attr(ode, "iov")$n_bins) && attr(ode, "iov")$n_bins > 1) {
      if(attr(ode, "iov")$n_bins != (length(iov_bins)-1)) {
        warning("Number of IOV bins specified for model does not match supplied `iov_bins` argument. This could lead to simulation failures or erroneous output.")
      }
    }
    if(is.null(analytical)) {
      if(inherits(ode, "function") && !isTRUE(attr(ode, "cpp"))) {
        stop("Sorry. Non-C++ functions are deprecated as input for ODE.")
      } else {
        if(inherits(ode, "function")) {
          size <- attr(ode,  "size")
        } else {
          size <- attr(get(ode),  "size")
        }
      }
    } else {
      size <- attr(analytical, "size")
    }
    if(is.null(ode) && is.null(analytical)) {
      stop("Please specify at least the required arguments 'ode' or 'analytical' for simulations.")
    }
    if(is.null(parameters) && is.null(parameters_table)) {
      stop("Please specify 'parameters' (or `parameters_table`) for the model.")
    }
    if(is.null(regimen)) {
      stop("Please specify a regimen created using the `new_regimen()` function.")
    }
    #  if(!is.null(t_tte)) {
    #    stop("Please specify possible observation times for time-to-event analysis as 't_tte' argument!")
    #  }
    if(is.null(analytical)) {
      if(inherits(ode, "function") && is.null(attr(ode, "cpp")) || attr(ode, "cpp") == FALSE) {
        stop("Sorry. Non-C++ functions are deprecated.")
      }
    }
    if(!is.null(covariates)) {
      if(!is.null(covariates_table)) {
        stop("Both `covariates` and `covariates_table` are specified!")
      }
      if(!inherits(covariates, "list")) {
        stop("Covariates need to be specified as a list!")
      } else {
        for(key in names(covariates)) {
          if(! inherits(covariates[[key]], "covariate")) {
            if(inherits(covariates[[key]], "numeric")) { # auto-conversion for convenience
              covariates[[key]] <- new_covariate(covariates[[key]])
            }
          }
        }
      }
    }
    if(!is.null(parameters_table)) {
      if(inherits(parameters_table, "data.frame") || inherits(parameters_table, "data.table")) {
        parameters_table <- table_to_list(parameters_table)
      }
      if(!inherits(parameters_table, "list")) {
        stop("Sorry, covariates for population seem to be misspecified. See manual for more information.")
      }
      if(length(parameters_table) != n_ind) {
        n_ind <- length(parameters_table)
      }
      p <- parameters_table[[1]]
    }
    if(!is.null(covariates_table)) {
      if(inherits(covariates_table, "data.frame") || inherits(covariates_table, "data.table")) {
        covariates_table <- covariates_table_to_list(covariates_table, covariates_implementation)
      }
      if(! inherits(covariates_table, "list")) {
        stop("Sorry, covariates for population seem to be misspecified. See manual for more information.")
      }
      if(length(covariates_table) != n_ind) {
        n_ind <- length(covariates_table)
      }
      message(paste0("Simulating ", n_ind, " individuals."))
    }
    ## check parameters specified
    if(is.null(analytical)) {
      pars_ode <- attr(ode, "parameters")
      rates <- paste0("rate[", 0:(size-1), "]")
      if(!all(pars_ode %in% c(names(p), rates))) {
        m <- match(c(names(p), names(covariates)), pars_ode)
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
        if(all(covs_ode != "") && !all(covs_ode %in% c(names(covariates_tmp)))) {
          m <- match(names(covariates_tmp), covs_ode)
          if(length(m) == 0) {
            missing <- covs_ode
          } else {
            missing <- covs_ode[-m[!is.na(m)]]
          }
          stop("Not all covariates for this model have been specified. Missing: \n  ", paste(missing, collapse=", "))
        }
      }
    }
    if(! (inherits(regimen, "regimen") || inherits(regimen, "regimen_multiple"))) {
      stop("Please create a regimen using the new_regimen() function!")
    }
    if(verbose) {
      message("Simulating...")
    }
  } else {
    size <- attr(ode, "size")
  }
  if (!is.null(omega)) {
    if(inherits(omega, "matrix")) {
      omega_mat <- omega
    }
    if(mode(omega) == "numeric") {
      omega_mat <- triangle_to_full(omega)
    }
    etas <- mvrnorm2(n = n_ind, mu=rep(0, nrow(omega_mat)), Sigma=omega_mat, sequence=sequence)
    if(n_ind == 1) {
      etas <- t(matrix(etas))
    }
  }
  if(inherits(regimen, "regimen_multiple")) {
    n_ind <- length(regimen)
  } else {
    if(is.null(t_obs)) { # find reasonable default to output
      t_obs <- get_t_obs_from_regimen(
        regimen, obs_step_size, t_max,
        covariates, extra_t_obs, t_init = t_init)
    }
    if(is.null(obs_type)) {
      obs_type <- rep(1, length(t_obs))
    }
    if(is.null(event_table)) {
      if(is.null(covariates_table)) {
        design <- create_event_table(regimen, t_max, t_obs, t_tte, t_init = t_init, p, covariates, model = ode, obs_type = obs_type)
      } else {
        design <- create_event_table(regimen, t_max, t_obs, t_tte, t_init = t_init, p, covariates[[1]], model = ode, obs_type = obs_type)
      }
      if(return_event_table) {
        return(design)
      }
    } else {
      design <- event_table
    }
    design_i <- design
    p$dose_times <- regimen$dose_times
    p$dose_amts  <- regimen$dose_amts
  }

  ## Use analytical equations (ADVAN):
  if(!is.null(analytical)) {
    ana_model <- advan(analytical, cpp = TRUE)
    simdata <- advan_create_data(regimen = regimen,
                                 parameters = parameters,
                                 cmts = attr(ana_model, "cmt"),
                                 t_obs = t_obs,
                                 covariates = covariates,
                                 covariate_model = covariate_model)
    res <- ana_model(simdata)
    out <- advan_parse_output(
      res,
      cmts = attr(ana_model, "cmt"),
      t_obs = t_obs,
      extra_t_obs = extra_t_obs,
      regimen = regimen)
    return(out)
  }

  ## Continue with ODE simulation
  if (is.null(A_init)) {
    A_init <- rep(0, size)
  }
  events <- c() # only for tte
  if(inherits(regimen, "regimen_multiple") && !is.null(covariates_table)) {
    stop("Sorry, can't simulate multiple regimens for a population in single call to PKPDsim. Use a loop instead.")
  }

  ## Set up mixture model
  use_mixture <- FALSE
  if(!is.null(attr(ode, "mixture"))) {
    use_mixture <- TRUE
    mixture_obj <- attr(ode, "mixture")[[1]]
    mixture_obj$parameter <- names(attr(ode, "mixture"))[[1]]
    if(!is.null(parameters_table)) {
      if(length(parameters_table) != length(mixture_group)) {
        stop("Length of `mixture_group` vector should be same as length of `parameters_table`.")
      }
    }
    if(!is.null(covariates_table)) {
      if(length(covariates_table) != length(mixture_group)) {
        stop("Length of `mixture_group` vector should be same as length of `covariates_table`.")
      }
    }
    if(is.null(mixture_group)) {
      if(verbose) message(paste0(
        "No `mixture_group` supplied, using value specified in `parameters` for ",
        mixture_obj$parameter, "."
      ))
    }
  }

  ## Override integrator step size if precision tied to model
  int_step_size <- ifelse(!is.null(attr(ode, "int_step_size")), as.num(attr(ode, "int_step_size")), int_step_size)
  comb <- vector(mode = "list", length = n_ind)
  for (i in 1:n_ind) {
    p_i <- p
    if(!is.null(covariates_table)) {
      covariates_tmp <- covariates_table[[i]]
      design_i <- create_event_table(regimen, t_max, t_obs, t_tte, t_init = t_init, p_i, covariates_table[[i]], model = ode, obs_type = obs_type)
    } else {
      covariates_tmp <- covariates
    }
    if(!is.null(parameters_table)) {
      p_i <- parameters_table[[i]]
    }
    if(use_mixture) {
      if(!is.null(mixture_group)) {
        p_i[[mixture_obj$parameter]] <- mixture_obj$values[mixture_group[i]]
      }
    }
    if(inherits(regimen, "regimen_multiple")) {
      if(is.null(t_obs)) { # find reasonable default to output
        t_obs <- get_t_obs_from_regimen(
          regimen[[i]], obs_step_size, t_max,
          covariates, extra_t_obs)
      }
      if(is.null(obs_type)) {
        obs_type <- rep(1, length(t_obs))
      }
      design_i <- create_event_table(regimen[[i]], t_max, t_obs, t_tte, t_init = t_init, p_i, covariates_tmp)
      if(inherits(regimen, "regimen_multiple")) {
        p_i$dose_times <- regimen[[i]]$dose_times
        p_i$dose_amts <- regimen[[i]]$dose_amts
      }
    }
    t_obs <- round(t_obs + t_init, 6) # make sure the precision is not too high, otherwise NAs will be generated when t_obs specified
    if (!is.null(omega)) {
      n_om <- nrow(omega_mat)
      if(length(omega_type) == 1) {
        omega_type <- rep(omega_type, n_om)
      } else {
        if(length(omega_type) != n_om) {
          warning("Length of `omega_type` vector is not equal to expected number of omega's. This could lead to errors or unexpected results.")
        }
      }
      if (any(omega_type == "exponential")) {
        idx <- (omega_type == "exponential")[1:n_om]
        p_i[(1:nrow(omega_mat))[idx]] <- (utils::relist(unlist(utils::as.relistable(p_i[1:nrow(omega_mat)])) * exp(etas[i,])))[idx]
      }
      if(any(omega_type == "normal")) {
        idx <- (omega_type == "normal")[1:n_om]
        p_i[(1:nrow(omega_mat))[idx]] <- utils::relist(unlist(utils::as.relistable(p_i[1:nrow(omega_mat)])) + etas[i,])[idx]
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
        obs_type = obs_type,
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

    tmp$y <- matrix(unlist(tmp$y), nrow = length(tmp$time), byrow = TRUE)
    tmp <- as.data.frame(tmp)

    dat_obs <- create_obs_data(
      ode_data = tmp,
      obs_attr = attr(ode, "obs"),
      id = i
    )

    if(only_obs || !is.null(analytical)) {
      dat_ind <- dat_obs
    } else {
      y_cols <- grep("^y", colnames(tmp))
      comp_labels <- gsub("y\\.", "", colnames(tmp)[y_cols])
      # If there's just one y column it's called y, not y.1; make sure it gets
      # labeled 1
      comp_labels <- gsub("y", "1", comp_labels)

      dat_ind <- reshape(
        tmp,
        direction = "long",
        varying = list(y_cols),
        v.names = "y",
        times = comp_labels,
        timevar = "comp"
      )
      dat_ind$id <- i
      dat_ind <- data.table::rbindlist(
        list(dat_ind, dat_obs),
        use.names = TRUE
      )
    }
    names(dat_ind)[names(dat_ind) == "time"] <- "t"

    ## Add parameters, variables and/or covariates, if needed. Implementation is slow, can be improved.
    ## Parameters
    if(!is.null(output_include$parameters) && output_include$parameters) {
      dat_ind <- cbind(
        dat_ind,
        # These should be single values, so ok to repeat them for every row in
        # the data
        as.data.frame(p_i[!names(p_i) %in% c("dose_times", "dose_amts", "rate")])
      )
    }
    ## Covariates
    cov_names <- NULL
    if(!is.null(output_include$covariates) && output_include$covariates && (!is.null(covariates) || !is.null(covariates_table))) {
      cov_names <- names(covariates_tmp)
      # List of data frames for each covariate
      calculated_covs <- lapply(cov_names, function(key) {
        calculated_cov <- unique(
          data.frame(
            t = design_i$t,
            # calculate covariate values based on time and gradient
            design_i[[paste0("cov_", key)]] + (design_i$t - design_i[[paste0("cov_t_", key)]]) * design_i[[paste0("gradients_", key)]]
          )
        )
        names(calculated_cov) <- c("t", key)
        calculated_cov
      })
      calculated_covs <- Reduce(
        function(x, y) merge(x, y, by = "t"),
        calculated_covs
      )
      dat_ind <- merge(dat_ind, calculated_covs, by = "t", all.x = TRUE)
    }

    ## Variables
    var_names <- attr(ode, "variables")
    var_names <- setdiff(var_names, cov_names)
    var_names <- var_names[var_names != "NULL"]
    if(is.null(output_include$variables) || isFALSE(output_include$variables)) {
      ## Drop variable names if not requested
      dat_ind[names(dat_ind) %in% var_names] <- NULL
    }

    comb[[i]] <- dat_ind
  }

  comb <- data.table::rbindlist(comb, use.names = TRUE)

  # Add concentration to dataset, and perform scaling and/or transformation:
  par_names <- NULL
  if(!is.null(output_include$parameters) && output_include$parameters) {
    par_names <- names(p_i)[!names(p_i) %in% c("dose_times", "dose_amts", "rate")]
  }
  all_names <- unique(c(par_names, cov_names, var_names))
  all_names <- intersect(all_names, names(comb)) # only cols that appear in data

  if(!extra_t_obs) {
    ## include the observations at which a bolus dose is added into the output object too
    comb <- comb[!duplicated(paste(comb$id, comb$comp, comb$t, comb$obs_type, sep="_")),]
  } else { # only remove duplicates at t=0
    comb <- comb[!(duplicated(paste(comb$id, comb$comp, comb$t, comb$obs_type, sep="_")) & comb$t == 0),]
  }
  grid <- expand.grid(paste(t_obs, obs_type, sep="_"), unique(comb$id), unique(comb$comp))
  colnames(grid) <- c("t_obs_type", "id", "comp")
  comb$t_obs_type <- paste(comb$t, comb$obs_type, sep = "_")
  comb$rownr <- 1:nrow(comb) # keep row-ordering during merge
  comb <- merge(grid, comb, all=FALSE)[, c("id", "t_obs_type", "t", "comp", "y", "obs_type", "rownr", all_names)]
  rm_cols <- which(colnames(comb) %in% c("t_obs_type", "rownr"))
  comb <- comb[order(comb$id, comb$comp, comb$t, comb$obs_type, comb$rownr, decreasing=FALSE), -rm_cols]
  if(!is.null(regimen_orig$ss_regimen)) {
    t_ss <- utils::tail(regimen_orig$ss_regimen$dose_times,1) + regimen_orig$ss_regimen$interval
    comb$t <- as.num(comb$t) - t_ss
    comb <- comb[comb$t >= 0,]
  }

  ## add residual variability
  if(!is.null(res_var)) {
    comb[comb$comp == 'obs',]$y <- add_ruv(
      x = comb[comb$comp == 'obs',]$y,
      ruv = res_var,
      obs_type = comb[comb$comp == 'obs',]$obs_type)
  }
  comb$t <- comb$t - t_init

  class(comb) <- c("PKPDsim_data", class(comb))
  attr(comb, "regimen") <- regimen_orig
  attr(comb, "ode_code") <- attr(ode, "code")
  attr(comb, "parameters") <- attr(ode, "parameters")
  return(comb)
}
