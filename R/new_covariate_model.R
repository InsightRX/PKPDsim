#' covariate model function
#'
#' @param model covariate model specified as list
#'
#' @export
new_covariate_model <- function(model = list()) {
  new_model <- list()
  for (i in seq(names(model))) {
    covariate_templ <- function(par, cov = list()) {
    }
    covariate_mod <- model[[names(model)[i]]]
    body(covariate_templ) <- covariate_mod
    new_model[[names(model)[i]]] <- covariate_templ
  }
  new_model
}
