#' Convert covariate table specified as data.frame
#'
#' Can handle time-varying data too, if `t` or `time` is specified as column
#'
#' @param covariates_table `data.frame`` with covariates in columns. Potentially with `id` and `t` columns
#' @param covariates_implementation `list` with implementation method per covariate
#' @export
#' @return List of covariates
covariates_table_to_list <- function(covariates_table, covariates_implementation = list()) {
  covs_obj <- list()
  names(covariates_table)[names(covariates_table) == "ID"] <- "id" # NONMEM syntax
  names(covariates_table)[names(covariates_table) == "TIME"] <- "t" # NONMEM syntax
  names(covariates_table)[names(covariates_table) == "time"] <- "t"
  if(!"id" %in% names(covariates_table)) {
    covariates_table$id <- 1:nrow(covariates_table)
    warning('No ID column provided; adding dummy IDs.')
  }
  if(!"t" %in% names(covariates_table)) {
    covariates_table$t <- 0
  }
  m <- match(c("id", "t"), colnames(covariates_table))
  covs <- colnames(covariates_table)[-m]
  ids <- unique(covariates_table$id)
  for(i in seq(ids)) {
    tmp <- covariates_table[covariates_table$id == ids[i],]
    l <- list()
    for(j in seq(covs)) {
      implementation <- "interpolate"
      if(!is.null(covariates_implementation[[covs[j]]])) {
        implementation <- covariates_implementation[[covs[j]]]
      }
      l[[covs[j]]] <- new_covariate(
        value = tmp[,covs[j]], 
        times = tmp$t, 
        implementation = implementation)
    }
    covs_obj[[i]] <- l
  }
  return(covs_obj)
}
