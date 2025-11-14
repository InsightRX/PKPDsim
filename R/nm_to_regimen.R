#' Create a regimen from NONMEM data
#'
#' Create a regimen based on a NONMEM, or NONMEM-like dataset
#' @param data NONMEM-type dataset
#' @param dose_cmts map from compartment number to dose type, defaults to compartment 1 being an infusion dose
#' @param reset_time start time for each simulated patient at 0, irrespective of design in dataset
#' @param first_only use only design from first individual in dataset
#' @export
#' @return Regimen object
nm_to_regimen <- function(
  data,
  dose_cmts = c("1" = "infusion"),
  reset_time = TRUE,
  first_only = FALSE
) {
  colnames(data) <- tolower(colnames(data))
  if(!"evid" %in% colnames(data)) {
    stop("EVID column is required in source dataset!")
  }
  if(! "amt" %in% colnames(data)) {
    stop("AMT column is required in source dataset!")
  }
  if(! "time" %in% colnames(data)) {
    stop("TIME column is required in source dataset!")
  }
  if(! "cmt" %in% colnames(data)) {
    stop("CMT column is required in source dataset!")
  }
  m <- match(c("id", "mdv", "evid", "amt", "time", "rate", "cmt"), colnames(data), 0)
  m <- m[m>0]
  data <- data[,m]
  doses <- data[data$evid == 1,]
  ids <- unique(doses$id)
  if(first_only) {
    ids <- ids[1]
  }
  reg <- list()
  for(i in 1:length(ids)) {
    tmp <- doses[doses$id == ids[i],]
    if(reset_time) {
      tmp$time <- tmp$time - min(tmp$time)
    }
    # map cmt to dose type
    reg[[i]] <- new_regimen(
      amt = tmp$amt,
      times = tmp$time,
      cmt = tmp$cmt,
      type = unname(unlist(dose_cmts[as.character(tmp$cmt)])),
      t_inf = ifelse(is.na(tmp$rate), NA, tmp$amt / tmp$rate)
    )
  }
  if(length(ids) == 1) {
    return(reg[[1]])
  } else {
    class(reg) <- c(class(reg), "regimen_multiple")
    return(reg)
  }
}
