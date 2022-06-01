#' defines C code for TDM before dose conditions
#'
#' Currently only available for 1-cmt and 2-cmt IV models
#'
#' @param def model definition, named recursive list with at least the
#' objects `misc$model_type`, `parameters` and `variables`
#' @return model defintion with `state_init` object added describing how to initializing the compartments.
#' @keywords internal
define_tdm_init_model <- function(def){
  if(!is.null(def$state_init)) {
    stop("Sorry, state init already specified. Cannot override for TDM-based initializiaton.")
  }
  if (!def$misc$model_type %in% c("1cmt_iv", "2cmt_iv")) {
    stop("Sorry, TDM initialization not supported for this model type yet.")
  }

  def$parameters$TDM_INIT <- 0
  V1 <- ifelse("Vi"  %in% def$variables, "Vi",  "V")
  V2 <- ifelse("V2i" %in% def$variables, "V2i", "V2")
  Q  <- ifelse("Qi"  %in% def$variables, "Qi",  "Q")

  if(def$misc$model_type == "1cmt_iv") {
    def$state_init <- paste0(" A[0] = TDM_INIT * ", V1, "; ")
  }
  if(def$misc$model_type == "2cmt_iv") {
    def$state_init <- paste0(
    "A[0] = TDM_INIT * ", V1, ";\ ",
    "A[1] = (", Q, "/", V1, ")*(TDM_INIT *", V1, ") / (", Q, "/", V2, "); "
    )
  }
  def
}
