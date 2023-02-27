#' Get expected variance/sd/ci of dependent variable
#' based on PKPDsim model, parameters, and regimen
#'
#' @param model model, created using `PKPDsim::new_ode_model()`
#' @param parameters parameters list
#' @param regimen regimen, as created using `PKPDsim::new_regimen()`
#' @param t_obs vector of observation times
#' @param obs_comp observation compartment. If NULL will be "obs" (default)
#' @param obs_variable observation variable. If NULL, will be ignored, otherwise will override `obs_comp`.
#' @param omega triangle omega block
#' @param omega_full full omega block
#' @param ruv residual variability, supplied as a named list, ex: `list(prop = 0, add = 0, exp = 0)`
#' @param y vector of observations. If NULL, then a new simulation will be performed.
#' @param rel_delta rel_delta
#' @param method method, `delta` or `sim`
#' @param sequence for simulations, if not NULL the pseudo-random sequence to use, e.g. "halton" or "sobol". See `mvrnorm2` for more details.
#' @param auc is AUC?
#' @param n_ind number of individuals to simulate with sim method
#' @param sd return as standard deviation (`TRUE`) or variance (`FALSE`)
#' @param q return vector of quantiles instead of sd/var. Will return parametric quantiles when delta-method is used, non-parametric for simulation-based methods.
#' @param in_parallel run simulations in parallel?
#' @param n_cores if run in parallel, on how many cores?
#' @param return_all return object with all relevant information?
#' @param ... passed on to `sim_ode()`
#'
#' @export
#' @return Vector of standard deviations or variances (or quantiles thereof) for dependent value
#'   variable
get_var_y <- function(
  model = NULL,
  parameters = list(),
  regimen = list(),
  t_obs = c(1:48),
  obs_comp = NULL,
  obs_variable = NULL,
  omega = c(0.1, 0.05, 0.1),
  omega_full = NULL,
  n_ind = NULL,
  ruv = NULL,
  y = NULL,
  rel_delta = 0.0001,
  method = "delta",
  sequence = NULL,
  auc = FALSE,
  sd = TRUE,
  q = NULL,
  in_parallel = FALSE,
  n_cores = 3,
  return_all = FALSE,
  ...) {
    if(method == "sim" && is.null(n_ind)) {
      stop("Please specify number of individuals when using simulation method!")
    }
    omega_full <- triangle_to_full(omega)
    n_par_est <- nrow(omega_full)
    parameters_est <- parameters[1:n_par_est]
    n_sim <- n_ind
    n_ind <- 1
    if(is.null(obs_comp)) {
      obs_comp <- "obs"
    }
    output_include <- NULL
    if(!is.null(obs_variable)) {
      output_include <- list(variables = TRUE, parameters = TRUE)
    }
    event_table <- sim_ode(
      ode = model,
      regimen = regimen,
      t_obs = t_obs,
      only_obs = FALSE,
      parameters = parameters,
      output_include = output_include,
      return_event_table = TRUE,
      ...
    )
    if(is.null(y)) {
      res <- PKPDsim::sim_ode(
        ode = model,
        regimen = regimen,
        only_obs = FALSE,
        t_obs = t_obs,
        parameters = parameters,
        event_table = event_table,
        checks = TRUE,
        output_include = output_include,
        ...)
      res <- res[res$comp == obs_comp,]
      if(!is.null(obs_variable)) {
        y <- res[[obs_variable]]
      } else {
        y <- res$y
      }
    }
    if(auc) y <- diff(y)
    nams <- names(parameters_est)
    jac <- c()
    if(!(method %in% c("delta", "sim"))) {
      stop("Requested method not recognized!")
    }
    types <- c("regular", "log")
    v <- list()
    qnt <- list()
    if(method == "delta") {
      sim_func <- function(i, ...) {
        par_tmp <- parameters
        dP <- rel_delta * par_tmp[[nams[i]]]
        par_tmp[[nams[i]]] <- par_tmp[[nams[i]]] + dP
        res_dP <- PKPDsim::sim_ode(
          ode = model,
          regimen = regimen,
          t_obs = t_obs,
          only_obs = FALSE,
          parameters = par_tmp,
          output_include = output_include,
          event_table = event_table,
          ...
        )
        res_dP <- res_dP[res_dP$comp == obs_comp,]
        if(!is.null(obs_variable)) {
          dy <- res_dP[[obs_variable]]
        } else {
          dy <- res_dP$y
        }
        if(auc) {
          dy <- diff(dy)
        }
        dydP <- list(
          regular = calc_dydP(dy, y, rel_delta, log_y = FALSE),
          log = calc_dydP(dy, y, rel_delta, log_y = TRUE)
        )
        return(dydP)
      }
      # running in parallel is actually not faster for most simple models. Only for models with larger number of parameters.
      if(in_parallel) {
        jac <- matrix(unlist(parallel::mclapply(seq(along = nams), sim_func, mc.cores = n_cores, ...)), nrow = ifelse(auc, 1, length(t_obs)))
      } else {
        jac <- matrix(unlist(lapply(seq(along = nams), sim_func, ...)), nrow = ifelse(auc, 1, length(t_obs)))
      }
      for(i in seq(types)) {
          type <- types[i]
          idx <- seq(from = i, to = length(nams)*2-(2-i), by = 2)
          jac_i <- matrix(jac[,idx], nrow=nrow(jac)) # force matrix, also for single row matrices
          v[[type]] <- diag(jac_i %*% omega_full %*% t(jac_i))
          if(!is.null(q)) {
            qnt[[type]] <- matrix(
              rep(res$y, each = length(q)) + rep(qnorm(q), length(q)) * rep(sqrt(v[[type]]), each = length(q)),
              nrow = length(q),
              byrow = FALSE
            )
          }
          if(!is.null(ruv)) {
            if(!is.null(ruv$exp)) {
              v[[type]] <- v[[type]] * exp(ruv$exp)^2
            }
            if(!is.null(ruv$prop)) {
              v[[type]] <- v[[type]] + (res$y * ruv$prop)^2
            }
            if(!is.null(ruv$add)) {
              v[[type]] <- v[[type]] + ruv$add^2
            }
          }
      }
    }
    if(method == "sim") {
      res_sim <- PKPDsim::sim_ode(ode = model,
                         omega = omega,
                         regimen = regimen,
                         t_obs = t_obs,
                         only_obs = FALSE,
                         parameters = parameters,
                         n_ind = n_sim,
                         sequence = sequence,
                         output_include = output_include,
                         ...)
      if(is.null(obs_comp)) {
        res_sim <- res_sim[res_sim$comp == "obs",]
      } else {
        res_sim <- res_sim[res_sim$comp == as.character(obs_comp),]
      }
      if(!is.null(ruv)) {
        if(!is.null(ruv$prop)) {
          res_sim$y <- res_sim$y + res_sim$y * rnorm(length(res_sim$y), 0, ruv$prop)
        }
        if(!is.null(ruv$add)) {
          res_sim$y <- res_sim$y + rnorm(length(res_sim$y), 0, ruv$add)
        }
        if(!is.null(ruv$exp)) {
          res_sim$y <- res_sim$y * exp(rnorm(length(res_sim$y), 0, ruv$exp))
        }
      }
      v <- list(
        regular = aggregate(res_sim$y, by = list(res_sim$t), FUN = "var")$x,
        log = aggregate(log(res_sim$y), by = list(res_sim$t), FUN = "var")$x
      )
      if(!is.null(q)) {
        tmp <- aggregate(res_sim$y, list(res_sim$t), quantile, q)
        tmp_log <- aggregate(log(res_sim$y), list(res_sim$t), quantile, q)
        qnt$regular <- t(tmp[,-1])
        colnames(qnt$regular) <- tmp[,1]
        rownames(qnt$regular) <- paste0("p_", q)
        qnt$log <- t(tmp_log[,-1])
        colnames(qnt$log) <- tmp_log[,1]
        rownames(qnt$log) <- paste0("p_", q)
      }
    }
    if(return_all) {
      obj <- list(pred = res,
                  regular = list(
                    sd   = sqrt(v$regular),
                    var  = v$regular
                  ),
                  log = list(
                    sd   = sqrt(v$log),
                    var  = v$log
                  ))
      if(!is.null(q)) {
        obj$q = qnt
      }
      if(method == "sim") {
        obj$sim <- res_sim
      }
      return(obj)
    } else {
      if(is.null(q)) {
        if(sd) {
          for(type in types) {
            v[[type]] <- sqrt(v[[type]])
          }
        }
        return(v)
      } else {
        return(qnt)
      }
    }
}

#' Calculate derivative
#'
#' @param dy dy
#' @param y dependent value
#' @param rel_delta relative delta
#' @param log_y logical indicating if the dependent variable is log transformed
calc_dydP <- function(dy, y, rel_delta, log_y) {
  if(log_y) {
    dy[dy <= 0] <- 1e-9
    y[y <= 0] <- 1e-9
    dydP <- (log(dy) - log(y)) / rel_delta
  } else {
    dydP <- (dy - y) / rel_delta
  }
  return(dydP)
}
