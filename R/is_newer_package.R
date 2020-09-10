#' Check if package number is different from currently installed,
#' and provide some messaging.
#'
#' Technically it only checks if a package version is different, not necessarily
#' a higher version number.
#'
#' @param package R package
#' @param new_version new version number
#'
is_newer_package <- function(package, new_version) {
    installed_version <- NULL
    tryCatch({
        installed_version <- utils::packageVersion(package)
    }, error = function(e) {
        return(TRUE)
    })
    if(is.null(installed_version)) {
        message(paste0("- Package ", package, " not installed yet."))
        return(TRUE)
    } else {
        if(installed_version == new_version) {
            message(paste0("- Installed version is already newest, skipping installation of ", package, "."))
            return(FALSE)
        } else {
            message(paste0("- Installed version is different from current version number, starting installation of ", package, "."))
            return(TRUE)
        }
    }
}
