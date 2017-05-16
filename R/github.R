
#' Download a single file from GitHub, or get list of files
#'
#' @param repo repository, specified as `"owner/repo"`
#' @param path path to file
#' @param branch git branch
#' @param auth auth_token (for private repositories)
#' @export
github_download <- function(repo,
                                 path = "/README.md",
                                 branch = "master",
                                 auth_token = NULL,
                                 raw = FALSE,
                                 action = "download") {
  full_url <- NULL
  if(!(action %in% c("download", "ls"))) {
    stop("Action not defined.")
  }
  if(action == "download") {
    full_url <- paste0("https://raw.githubusercontent.com/", repo, "/", branch, "/", path)
  }
  if(action == "ls") {
    full_url <- paste0("https://api.github.com/repos/", repo, "/contents/", path)
  }
  auth <- NULL
  if(!is.null(auth_token)) {
    auth <- httr::authenticate(
      user = auth_token,
      password = "x-oauth-basic",
      type = "basic"
    )
  }
  req <- httr::GET(full_url, auth)
  if(is.null(req$status_code)) {
    stop("Unknown error.")
  } else {
    if(req$status_code == 404) {
      stop("Sorry, resource not found, or not authorized.")
    } else {
      if(req$status_code != 200) {
        stop(paste0("Sorry, some error occurred (", req$status_code, ")."))
      }
    }
  }
  obj <- NULL
  tryCatch({
    obj <- readBin(req$content, "character")
  })
  if(raw) {
    return(obj)
  } else {
    def <- jsonlite::fromJSON(
      obj %>%
        stringr::str_replace_all("'", "\"") %>%
        stringr::str_replace_all("\\\\n", "\n") %>%
        stringr::str_replace_all("\n", "") %>%
        stringr::str_replace_all("\\\\", "\\\\n")
    )
    return(def)
  }
}
