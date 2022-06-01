#' Check that mixture model is specified in right format and within constraints (1 parameter, 2 groups)
#'
#' @param mixture mixture model specification (as list, e.g. `list("CL" = list(values=c(5, 10), probability=0.3))`)
#' @param parameters vector of parameter names
#' @keywords internal
check_mixture_model <- function(mixture, parameters) {
  if(!is.null(mixture)) {
    if(length(names(mixture)) > 1) {
      stop("Currently only mixture models for a single parameter are allowed.")
    }
    if(!names(mixture) %in% parameters) {
      stop("Parameter for mixture model needs to be an existing model parameter.")
    }
    if(is.null(mixture[[1]]$values) || is.null(mixture[[1]]$probability)) {
      stop("Mixture needs to be specified by `values` and `probability.`")
    }
    if(length(mixture[[1]]$values) != 2) {
      stop("Currently only mixture models for two groups are allowed. Please provide only 2 values for the mixture model.")
    }
    if(mixture[[1]]$probability < 0 || mixture[[1]]$probability > 1) {
      stop("Mixture probability needs to be between 0 and 1.`")
    }
  }
}
