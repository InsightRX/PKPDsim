#' Create obs data
#'
#' Used by [sim()] to arrange data from ode() function into the correct format.
#'
#' @param ode_data data frame of output from ode() function
#' @param obs_attr "obs" attribute from ode() function
#' @param id ID of the individual
#' @seealso [sim()]
#' @md
create_obs_data <- function(ode_data, obs_attr, id) {
  if(!is.null(obs_attr) && length(obs_attr$cmt) > 1) {
    lab <- vapply(seq_along(obs_attr$cmt), function(x) {
      ifelse(!is.null(obs_attr$label[x]), obs_attr$label[x], paste0("obs", x))
    }, FUN.VALUE = character(1))

    obs_cols <- grep("^obs\\d", colnames(ode_data))
    dat_obs <- reshape(
      ode_data,
      direction = "long",
      varying = obs_cols,
      v.names = "y",
      times = lab,
      timevar = "comp"
    )
    dat_obs$id <- id
  } else {
    dat_obs <- ode_data
    dat_obs$id <- id
    dat_obs$comp <- "obs"
    dat_obs$y <- dat_obs$obs
  }
  dat_obs[, grep("^y\\.", colnames(dat_obs))] <- NULL # drop y.[number] cols
  dat_obs
}
