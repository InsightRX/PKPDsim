#' @export
model <- function(mod = NULL) {
  ret <- get("sim_wrapper_cpp", envir = as.environment("package:[MODULE]"))
  attr(ret, "cpp") <- TRUE
  attr(ret, "size") <- [N_COMP]
  attr(ret, "obs") <- list("cmt" = [OBS_COMP], "scale" = "[OBS_SCALE]", "variable" = [OBS_VARIABLE])
  attr(ret, "dose") <- list("cmt" = [DOSE_COMP], "bioav" = [DOSE_BIOAV], "duplicate" = [DOSE_DUPLICATE] )
  attr(ret, "code") <- "[CODE]"
  attr(ret, "pk_code") <- "[PK_CODE]"
  attr(ret, "state_init") <- [STATE_INIT]
  attr(ret, "parameters") <- [PARS]
  attr(ret, "fixed") <- [FIXED]
  attr(ret, "reparametrization") <- [REPARAM]
  attr(ret, "mixture") <- [MIXTURE]
  attr(ret, "variables") <- [VARS]
  attr(ret, "covariates") <- [COVS]
  attr(ret, "lagtime") <- [LAGTIME]
  attr(ret, "ltbs") <- [LTBS]
  attr(ret, "misc") <- [MISC]
  attr(ret, "cmt_mapping") <- [CMT_MAPPING]
  attr(ret, "int_step_size") <- [INT_STEP_SIZE]
  attr(ret, "iov") <- list([IOV])
  attr(ret, "cmt_mapping") <- [CMT_MAPPING]
  attr(ret, "nonmem") <- "[NONMEM]"
  attr(ret, "comments") <- "[COMMENTS]"
  attr(ret, "version") <- "[VERSION]"
  class(ret) <- c("PKPDsim", class(ret))
  return(ret)
}
