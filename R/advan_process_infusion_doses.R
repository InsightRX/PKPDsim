#' Add column RATEALL to ADVAN-style dataset to handle infusions
#'
#' Function adapted from code from Abuhelwa, Foster, Upton JPET 2015.
#' cleaned up and somewhat optimized. Can potentially be optimized more.
#'
#' @param data ADVAN-style dataset, e.g. created using `advan_create_data`.
#' @references Abuhelwa, A. Y., Foster, D. J. R., Upton, R. N. (2015)
#'   ADVAN-style analytical solutions for common pharmacokinetic models. J
#'   Pharmacol Toxicol Methods 73:42-8. DOI: 10.1016/j.vascn.2015.03.004
#' @export
#' @return Data frame containing additional RATEALL column.
advan_process_infusion_doses <- function (data) {

  # Calculate all amounts
  doserows <- data[data$AMT != 0, ]
  dosecount <- nrow(doserows)  #total number of doses
  doserows$DNUM <- 1:dosecount

  # Need to add times for ending the infusions
  doserowslast <- doserows
  doserowslast$TIME <- doserowslast$TIME + doserowslast$AMT/doserowslast$RATE
  doserowslast$DNUM <- -doserowslast$DNUM
  badcols <- which(!names(doserowslast) %in% c("ID","TIME","AMT","RATE","DV","DNUM"))
  doserowslast[, badcols] <- NA

  # Are there any doserows without a DV value?  These need to precede the infusion change
  noDVindex <- which(!doserowslast$TIME %in% data$TIME)
  doserowslastnoDV <- doserowslast[noDVindex,]
  doserowslastnoDV$AMT <- 0
  doserowslastnoDV$RATE <- 0
  doserowslastnoDV$DNUM <- NA

  # Collect the new rows
  doserows <- rbind(doserows, doserowslast, doserowslastnoDV)

  # Rewrite previous dose rows with new dose rows
  data$DNUM <- NA
  data <- rbind(data[data$AMT==0,], doserows)
  data <- data[order(data$ID, data$TIME, data$AMT), ]

  # Set an extra last row
  lastrow <- utils::tail(data, 1)
  lastrow$TIME <- lastrow$TIME+1
  data <- rbind(data, lastrow)

  # Now fill in the gaps for the covariates by locf
  data[, badcols] <- apply(data[, badcols], 2, na_locf)

  # Process infusion doses in a loop
  data$RATEALL <- 0
  for (DCOUNT in 1:dosecount) {
    data$RATEALLI <- 0
    data$DNUMI <- data$DNUM
    data$DNUMI[abs(data$DNUM) != DCOUNT] <- NA
    data$DNUMI <- na_locf(data$DNUMI)
    data$DNUMI[is.na(data$DNUMI)] <- 0
    data$RATEALLI[data$DNUMI==DCOUNT] <- data$RATE[which(data$DNUM==DCOUNT)]
    data$RATEALL <- data$RATEALL+data$RATEALLI
  }

  # This is crucial
  data$RATEALL[data$DNUM > 0] <- 0

  # Get rid of extra dose rows
  data <- data[data$DNUM > 0 | is.na(data$DNUM),]
  data <- data[, ! names(data) %in% (c("DNUM", "RATEALLI", "DNUMI"))]

  return(data)
}
