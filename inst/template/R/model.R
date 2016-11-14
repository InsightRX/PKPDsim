#' @export
model <- function(mod = NULL) {
  ret <- get("sim_wrapper_cpp", envir = as.environment("package:[MODULE]"))
  attr(ret, "cpp") <- TRUE
  attr(ret, "size") <- [N_COMP]
  attr(ret, "obs") <- list("cmt" = [OBS_COMP], "scale" = "[OBS_SCALE]")
  attr(ret, "dose") <- list("cmt" = [DOSE_COMP], "bioav" = [DOSE_BIOAV] )
  attr(ret, "code") <- "[CODE]"
  attr(ret, "pk_code") <- "[PK_CODE]"
  attr(ret, "parameters") <- [PARS]
  attr(ret, "variables") <- [VARS]
  attr(ret, "covariates") <- [COVS]
  # attr(ret, "lagtime") <- "[LAGTIME]"
  class(ret) <- c("PKPDsim", class(ret))
  return(ret)
}
