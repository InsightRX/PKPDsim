#' Install default literature model
#'
#' A very lightweight wrapper for `model_from_api` that installs previously
#' published models packaged within PKPDsim.
#'
#' @param model Name of model, e.g., "pk_busulfan_mccune". See
#' [available_default_literature_models()]
#' @param ... arguments passed onto `model_from_api`. For fine-grain control, it
#'   is better to install models directly from [model_from_api()] or
#'   [new_ode_model()].
#' @examples \dontrun{
#' install_default_literature_model("pk_busulfan_mccune")
#' }
#' @export

install_default_literature_model <- function(model, ...) {
  if (model == "all") {
    for (m in available_default_literature_models()) {
      install_default_literature_model(m, ...)
    }
  } else if (!model %in% available_default_literature_models()) {
    stop("model not found! see available_default_literature_models()")
  } else {
    model_from_api(
      system.file(
        file.path("models", paste0(model, ".json5")),
        package = "PKPDsim"
      ),
      install_all = TRUE,
      to_package = TRUE
    )
  }
}

#' See models from the literature available for installation
#'
#' @returns Returns a character vector of models available for installation
#' @examples
#' available_default_literature_models()
#' @export

available_default_literature_models <- function() {
  gsub("\\.json5", "", list.files(system.file("models", package = "PKPDsim")))
}
