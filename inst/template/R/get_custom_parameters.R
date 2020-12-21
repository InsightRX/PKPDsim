#' Import a set of custom parameters and some meta-data
#'
#' @param id label / id for a set of custom parameters
#'
#' @export
get_custom_parameters <- function(id = NULL) {
  if(!is.null(id)) {
    md_file <- system.file(package = utils::packageName(), paste0("md/custom_parameters.json"))
    if(file.exists(md_file)) {
      md <- jsonlite::fromJSON(md_file, flatten=FALSE)
      if(!is.null(md[[id]])) {
        pars <- list()
        to_list <- c("parameters", "iov", "ruv")
        for(l in to_list) {
            if(!is.null(md[[id]]$custom[[l]])) {
                pars[[l]] <- as.list(md[[id]]$custom[[l]])
            }
        }
        if(!is.null(md[[id]]$custom$omega_matrix)) {
            pars$omega_matrix <- md[[id]]$custom$omega_matrix[[1]]
        }
        pars$metadata <- list()
        meta <- c("label", "version", "population", "n_patients")
        for(key in meta) {
            if(!is.null(md[[id]][[key]])) {
                pars$metadata[[key]] <- md[[id]][[key]]
            }
        }
        return(pars)
      } else {
        warning(paste0("Custom parameter set not found. Available: ", paste(names(md), collapse=", ")))
      }
    } else {
        warning("No custom parameter set(s) defined for this package.")
    }
  }
}
