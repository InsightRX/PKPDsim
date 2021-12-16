#' Convert a table to a list
#'
#' @param table data.frame
#' @export
#' @return List containing original table contents
table_to_list <- function(table) {
  lapply(split(table, seq_along(table[,1])), as.list)
}
