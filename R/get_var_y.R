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
#' @param ruv residual varibility (list with `prop`, `add`, and/or `exp` arguments)
#' @param rel_delta rel_delta
#' @param method method, `delta` or `sim`
#' @param sequence for simulations, if not NULL the pseudo-random sequence to use, e.g. "halton" or "sobol". See `mvrnorm2` for more details.
#' @param log_y log-transform y value?
#' @param auc is AUC?
#' @param n_ind number of individuals to simulate with sim method
#' @param sd return as standard deviation (`TRUE`) or variance (`FALSE`)
#' @param q return vector of quantiles instead of sd/var. Will return parametric quantiles when deltamethod is used, non-parametric for simulation-basesd methods.
#' @param in_parallel run simulations in parallel?
#' @param n_cores if run in parallel, on how many cores?
#' @param return_all return object with all relevant information?
#' @param ... passed on to `sim_ode()`
#' 
#' @export
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
  ruv = list(prop = 0, add = 0, exp = 0),
  rel_delta = 0.0001,
  method = "delta",
  sequence = NULL,
  log_y = FALSE,
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
    res <- PKPDsim::sim_ode(
      ode = model,
      regimen = regimen,
      only_obs = FALSE,
      t_obs = t_obs,
      parameters = parameters,
      checks = TRUE,
      output_include = output_include,
      ...)
    res <- res[res$comp == obs_comp,]
    if(!is.null(obs_variable)) {
      y <- res[[obs_variable]]
    } else {
      y <- res$y
    }
    if(auc) y <- diff(y)
    nams <- names(parameters_est)
    jac <- c()
    if(!(method %in% c("delta", "sim"))) {
      stop("Requested method not recognized!")
    }
    if(method == "delta") {
      sim_func <- function(i, ...) {
        par_tmp <- parameters
        dP <- rel_delta * par_tmp[[nams[i]]]
        par_tmp[[nams[i]]] <- par_tmp[[nams[i]]] + dP
        res_dP <- PKPDsim::sim_ode(ode = model,
                          regimen = regimen,
                          t_obs = t_obs,
                          only_obs = FALSE,
                          parameters = par_tmp,
                          output_include = output_include,
                          ...)
        res_dP <- res_dP[res_dP$comp == obs_comp,]
        if(!is.null(obs_variable)) {
          dy <- res_dP[[obs_variable]]
        } else {
          dy <- res_dP$y
        }
        if(auc) {
          dy <- diff(dy)
        }
        if(log_y) {
          dy[dy <= 0] <- 1e-9
          y[y <= 0] <- 1e-9
          dydP <- (log(dy) - log(y)) / rel_delta
        } else {
          dydP <- (dy - y) / rel_delta
        }
        return(dydP)
      }
      # running in parallel is actually not faster for most simple models. Only for models with larger number of parameters.
      if(in_parallel) {
        jac <- matrix(unlist(parallel::mclapply(seq(along = nams), sim_func, mc.cores = n_cores, ...)), nrow = ifelse(auc, 1, length(t_obs)))
      } else {
        jac <- matrix(unlist(lapply(seq(along = nams), sim_func, ...)), nrow = ifelse(auc, 1, length(t_obs)))
      }
      v <- diag(jac %*% omega_full %*% t(jac))
      if(!is.null(q)) {
        qnt <- matrix(rep(res$y, each = length(q)) + rep(qnorm(q), length(q)) * rep(v, each = length(q)), ncol=length(q), byrow=TRUE)
      }
      if(!is.null(ruv)) {
        if(!is.null(ruv$exp)) {
          v <- v * exp(ruv$exp)^2
        }
        if(!is.null(ruv$prop)) {
          v <- v + (res$y * ruv$prop)^2
        }
        if(!is.null(ruv$add)) {
          v <- v + ruv$add^2
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
      v <- aggregate(res_sim$y, by = list(res_sim$t), FUN = "var")$x
      if(!is.null(q)) {
        tmp <- aggregate(res_sim$y, list(res_sim$t), quantile, q)
        qnt <- t(tmp[,-1])
        colnames(qnt) <- tmp[,1]
        rownames(qnt) <- paste0("p_", q)
      }
    }
    if(return_all) {
      obj <- list(pred = res,
                  sd   = sqrt(v),
                  var  = v)
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
          v <- sqrt(v)
        }
        return(v)
      } else {
        return(qnt)
      }
    }
}
