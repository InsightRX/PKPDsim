#' Read model definition from JSON
#'
#' Does some substitution of escaped characters in strings in the JSON file,
#' then converts to a list with [jsonlite::fromJSON()]
#'
#' @param path Path to JSON file
#' @md
#' @export
#' @return List containing contents of original JSON file
read_model_json <- function(path) {
  lines <- paste(readLines(path), collapse = "\n") %>%
    stringr::str_replace_all("'", "\"") %>%
    stringr::str_replace_all("\\\\n", "\n") %>%
    stringr::str_replace_all("\n", "") %>%
    stringr::str_replace_all("\\\\", "\\\\n")
  def <- jsonlite::fromJSON(lines)
}
