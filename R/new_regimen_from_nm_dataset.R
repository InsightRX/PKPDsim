#' Create a regimen from NM data
#'
#' Create a regimen based on a NONMEM, or NONMEM-like dataset
#' @param data NONMEM-type dataset
#' @param reset_time start time for each simulated patient at 0, irrespective of design in dataset
#' @param first_only use only design from first individual in dataset
#' @export
new_regimen_from_nm_dataset <- function(data,
                                        reset_time = TRUE,
                                        first_only = FALSE) {
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
  m <- match(c("id", "mdv", "evid", "amt", "time", "rate"), colnames(data), 0)
  m <- m[m>0]
  data <- data[,m]
  doses <- data[data$evid == 1,]
  dum <- data[data$evid == 2,]
  ids <- unique(doses$id)
  if(first_only) {
    ids <- ids[1]
  }
  type <- "bolus"
  if("rate" %in% colnames(doses) &! 0 %in% doses$rate) {
    type <- "infusion"
  }
  reg <- list()
  for(i in 1:length(ids)) {
    tmp <- doses %>% dplyr::filter(id == ids[i])
    if(reset_time) {
      tmp$time <- tmp$time - min(tmp$time)
    }
    if(type == "infusion") {
      reg[[i]] <- new_regimen(amt = tmp$amt, times = tmp$time, type = "infusion", t_inf = tmp$amt / tmp$rate)
    } else {
      reg[[i]] <- new_regimen(amt = tmp$amt, times = tmp$time, type = "bolus")
    }
  }
  if(length(ids) == 1) {
    return(reg[[1]])
  } else {
    class(reg) <- c(class(reg), "regimen_multiple")
    return(reg)
  }
}
