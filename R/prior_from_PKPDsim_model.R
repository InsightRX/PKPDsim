#' Get prior (population estimates) PKPDsim model package
#'
#' @param model package name
#' @param map remap model parameters from Stan to PKPDsim syntax, specified as a
#'   list. E.g. `map = list(V1 = V)`
#' @param drop vector of parameter names that we are not interested in or don't
#'   need to provide.
#'
#' @export
prior_from_PKPDsim_model <- function(
  model,
  map = list(),
  drop = c()
) {
  model <- gsub("_", "", model)
  par <- get("parameters", asNamespace(model))()
  par <- remap(par, map, reverse = TRUE)
  par <- par[!names(par) %in% drop]
  attr(par, "units") <- NULL
  par
}
