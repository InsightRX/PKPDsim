#' Convert covariate table specified as data.frame
#' Can handle time-varying data too, if `t` or `time` is specified as column
#'
#' @param covariates_table `data.frame`` with covariates in columns. Potentially with `id` and `t` columns
#' @param covariates_implementation `list` with implementation method per covariate
#' @export
covariate_table_to_list <- function(covariates_table, covariates_implementation = list()) {
  covs <- list()
  names(cov_table)[names(cov_table) == "ID"] <- "id" # NONMEM syntax
  names(cov_table)[names(cov_table) == "TIME"] <- "t" # NONMEM syntax
  names(cov_table)[names(cov_table) == "time"] <- "t"
  if(!"id" %in% names(cov_table)) {
    cov_table$id <- 1:length(cov_table)
  }
  if(!"t" %in% names(cov_table)) {
    cov_table$t <- 0
  }
  m <- match(c("id", "t"), colnames(cov_table))
  ids <- unique(cov_table$id)
  for(i in seq(ids)) {
    tmp <- cov_table[cov_table$id == ids[i],]
    cov_tmp <- tmp[, -m]
    l <- list()
    for(j in seq(names(cov_tmp))) {
      implementation <- "interpolate"
      if(!is.null(covariates_implementation[[names(cov_tmp)[j]]])) {
        implementation <- covariates_implementation[[names(cov_tmp)[j]]]
      }
      l[[names(cov_tmp)[j]]] <- new_covariate(value = cov_tmp[,j], times = tmp$t, implementation = implementation)
    }
    covs[[i]] <- l
  }
  return(covs)
}
