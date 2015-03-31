#' Simulate ODE
#'
#' Simulates a specified ODE system and regimen
#' @param ode function describing the ODE system
#' @param parameters
#' @param omega vector describing the lower-diagonal of the between-subject variability matrix
#' @param omega_type exponential or normal
#' @param n_ind number of individuals to simulate
#' @param regimen a regimen object created using the regimen() function
#' @param adherence List specifying adherence. Simulates adherence using either markov model or binomial sampling.
#' @param A_init vector with the initial state of the ODE system
#' @param step_size the step size between the observations (NOT the step size of the differential equation solver)
#' @param tmax maximum simulation time, if not specified will pick the end of the regimen as maximum
#' @param output vector specifying which compartment numbers to output
#' @return a data frame of compartments with associated concentrations at requested times
#' @export
#' @seealso \link{sim_ode_shiny}
#' @examples
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
                     step_size = .25,
                     tmax = NULL,
                     output_cmt = NULL
                     ) {
  if (!is.null(covariate_model) && !is.null(covariates)) {
    n_ind <- length(t(covariates[,1]))
    message(paste0("Simulating ", n_ind, " individuals with covariates...\n"))
    if (sum("tbl_df" %in% class(covariates))==0) {
      covariates <- data_frame(covariates)
    }
  }
  if ((!is.null(covariate_model) && is.null(covariates)) | (is.null(covariate_model) && !is.null(covariates))) {
    stop("For models with covariates, specify both the 'covariate_model' and the 'covariates' arguments. See help for more information.")
  }
  if (!is.null(dde)) {
    lsoda_func <- deSolve::dede
  } else {
    lsoda_func <- deSolve::lsoda
  }
  if(!is.null(dde)) {
    ode <- dde
  }
  if (class(ode) == "character") {
    ode <- get(ode)
  } else {
    warning("The preferred input to the 'ode' argument is a character string referencing the function, not the function itself.")
  }
  if(is.null(ode) | is.null(parameters)) {
    stop("Please specify at least the required arguments 'ode' and 'parameters'.")
  }
  if(is.null(regimen)) {
    regimen <- new_regimen()
  }
  # get the ODE size
  size <- get_size_ode(ode, parameters)

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
  if(class(regimen) != "regimen") {
    stop("Please create a regimen using the new_regimen() function!")
  }
  if(regimen$type == "infusion") {
    p$t_inf <- regimen$t_inf
    p$dose_type <- "infusion"
    design <- data.frame(rbind(cbind(t=regimen$dose_times, dose = regimen$dose_amts, dum = 0),
                               cbind(t=regimen$dose_times + regimen$t_inf, dose=0, dum = 1))) %>%
      dplyr::arrange(t)
  } else {
    p$dose_type <- "bolus"
    design <- data.frame(rbind(cbind(t=regimen$dose_times, dose = regimen$dose_amts, dum = 0)))
  }
  if (is.null(tmax)) {
    if (length(design$t) > 1) {
      tmax <- tail(design$t,1) + max(diff(regimen$dose_times))
    } else {
      tmax <- tail(design$t,1) + 24 # guess timeframe, user should use tmax argument
    }
  }
  design <- rbind(design %>%
                    dplyr::filter(t < tmax), tail(design,1))
  design[length(design[,1]), c("t", "dose")] <- c(tmax,0)
  times <- seq(from=0, to=tail(design$t,1), by=step_size)
  if (is.null(A_init)) {
    A_init <- rep(0, size)
  }
  p$dose_times <- regimen$dose_times
  p$dose_amts <- regimen$dose_amts
  if(!is.null(p$F)) {
    design$dose <- design$dose * F
  }
  scale <- 1
  if(class(attr(ode, "obs")[["scale"]]) == "numeric") {
    scale <- attr(ode, "obs")[["scale"]]
  }
  comb <- c()
  for (i in 1:n_ind) {
    p_i <- p
    if (!is.null(covariates) && !is.null(covariate_model)) {
      keys <- names(p_i)[names(p_i) %in% names(covariate_model)]
      if (length(keys) > 0) {
        for (j in seq(keys)) {
          p_i[[keys[j]]] <- covariate_model[[keys[j]]](par = p_i[[keys[j]]], cov = covariates[i,])
        }
      }
    }
    design_i <- design
    A_init_i = A_init
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
    if(class(A_init) == "function") {
      A_init_i = A_init(p_i)
    }
    tmp <- c()
    for (k in 1:(length(design$t)-1)) {
      if (k > 1) {
        A_upd <- dat[dat$t==tail(time_window,1),]$y
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
      dat <- num_int_wrapper (time_window, A_upd, ode, p_i, lsoda_func)
      tmp <- rbind(tmp, cbind(id = i, dat))
    }
    # Add concentration to dataset:
    if(!is.null(attr(ode, "obs"))) {
      if(class(attr(ode, "obs")[["scale"]]) == "character") {
        scale <- p_i[[attr(ode, "obs")[["scale"]]]]
      }
      tmp <- rbind (tmp, tmp %>% dplyr::filter(comp == attr(ode, "obs")[["cmt"]]) %>% dplyr::mutate(comp = "obs", y = y/scale))
    }
    comb <- rbind(comb, tmp)
  }
  if(!is.null(output_cmt)) {
    comb <- comb %>% dplyr::filter(comp %in% output_cmt)
  }
  return(data.frame(comb))
}
