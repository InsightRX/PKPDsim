#' Read model definition from JSON
#'
#' Does some substitution of escaped characters in strings in the JSON file,
#' then converts to a list with [jsonlite::fromJSON()]
#'
#' @param path Path to JSON file
#' @md
#' @export
#' @return List containing contents of original JSON file
read_model_json <- function(
  path
) {
  if(!file.exists(path)) {
    warning(paste0("Model information not found (", path, ")."))
    return()
  }
  ## Convert the multi-line objects in JSON5 to conventional JSON
  lines <- paste(readLines(path), collapse = "\n") %>%
    stringr::str_replace_all("'", "\"") %>%
    stringr::str_replace_all("\\\\n", "\n") %>%
    stringr::str_replace_all("\n", "") %>%
    stringr::str_replace_all("\\\\", "\\\\n")
  jsonlite::fromJSON(lines)
}
