#' Test a model
#'
#' @inheritParams model_from_api
#' @param test_file Path to a .R file containing tests to run
#' @param package Package name
#' @param force Run tests even if model is not flagged for building? Defaults to
#'   FALSE
#' @export
#' @return Runs test file for a model but does not return a value
test_model <- function(url, test_file, package, force = FALSE) {
  def <- read_model_json(url)
  if (isTRUE(def$build) || isTRUE(force)) {
    if (!file.exists(test_file)) {
      stop(paste0("Test file (", test_file,") not found!"))
    }
    source(test_file, local = TRUE)
    pkg_str <- paste0("package:", package)
    if (any(grepl(pkg_str, search()))) {
      detach(pkg_str, unload = TRUE, character.only = TRUE)
    }
  }
}
