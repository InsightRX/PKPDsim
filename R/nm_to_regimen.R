#' Create a regimen from NONMEM data
#'
#' Create a regimen based on a NONMEM, or NONMEM-like dataset
#' @param data NONMEM-type dataset
#' @param reset_time start time for each simulated patient at 0, irrespective of design in dataset
#' @param first_only use only design from first individual in dataset
#' @export
#' @return Regimen object
nm_to_regimen <- function(
  data,
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
    # use CMT if it exists in input data
    if (!is.null(tmp$cmt)){
      # if RATE is given, use to calculate infusion time t_inf
      if (!is.null(tmp$rate)){
        suppressWarnings({
          reg[[i]] <- new_regimen(
            amt = tmp$amt,
            times = tmp$time,
            cmt = tmp$cmt,
            t_inf = ifelse(tmp$rate == 0, 0, tmp$amt/tmp$rate)
          )
        })
      } else {
        # if no RATE is given, then assume bolus
        suppressWarnings({
          reg[[i]] <- new_regimen(
            amt = tmp$amt,
            times = tmp$time,
            cmt = tmp$cmt
          )
        })
      }
    } else if ("rate" %in% colnames(doses) &! 0 %in% doses$rate){
      # if rate exists and is non-zero, assume infusion
      reg[[i]] <- new_regimen(
        amt = tmp$amt,
        times = tmp$time,
        type = "infusion",
        t_inf = tmp$amt / tmp$rate
      )
    } else {
      # assume bolus
      reg[[i]] <- new_regimen(
        amt = tmp$amt,
        times = tmp$time,
        type = "bolus"
      )
    }
  }
  if(length(ids) == 1) {
    return(reg[[1]])
  } else {
    class(reg) <- c(class(reg), "regimen_multiple")
    return(reg)
  }
}
