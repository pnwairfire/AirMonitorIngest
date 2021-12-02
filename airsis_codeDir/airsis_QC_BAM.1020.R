#' @export
#' @importFrom rlang .data
#' @importFrom MazamaCoreUtils logger.trace logger.debug logger.warn logger.error
#'
#' @title Apply quality control to raw AIRSIS BAM.1020 dataframe
#'
#' @param tbl single site tibble created by \code{airsis_parseData()}
#' @param valid_longitude range of valid Longitude values
#' @param valid_latitude range of valid Latitude values
#' @param valid_flow range of valid Flow values
#' @param valid_AT range of valid AT values
#' @param valid_RHi range of valid RHi values
#' @param valid_pm25 range of valid ConcHr values
#' @param flagAndKeep flag, rather than remove, bad data during the QC process
#'
#' @description Perform various QC measures on AIRSIS data originally in
#' BAM.1020 format.
#'
#' Records with longitude or latitude == 0 are removed.
#'
#' @return Cleaned up tibble of AIRSIS monitor data.

airsis_QC_BAM.1020 <- function(
  tbl,
  valid_longitude = c(-180, 180),
  valid_latitude = c(-90, 90),
  valid_flow = c(.834*.95, .834*1.05),
  valid_AT = c(-Inf, 45),
  valid_RHi = c(-Inf, 45),
  valid_pm25 = c(-Inf, 5000),
  flagAndKeep = FALSE
) {

  logger.debug(" ----- airsis_BAM1020QualityControl() ----- ")

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(tbl)

  # > head(tbl)
  # # A tibble: 6 × 8
  #   datetime            longitude latitude  flow    AT   RHi  pm25 voltage
  #   <dttm>                  <dbl>    <dbl> <dbl> <dbl> <dbl> <dbl>   <dbl>
  # 1 2013-05-22 21:00:00     -120.     37.7 0      15.4    40   995      NA
  # 2 2013-05-22 22:00:00     -120.     37.7 0      19.5    30   995      NA
  # 3 2013-05-22 23:00:00     -120.     37.7 0.834  19.5    13     1      NA
  # 4 2013-05-23 00:00:00     -120.     37.7 0.834  19.5     9     4      NA
  # 5 2013-05-23 01:00:00     -120.     37.7 0.834  18.9     8     6      NA
  # 6 2013-05-23 02:00:00     -120.     37.7 0.834  17.6     8     3      NA

  # ----- Setup for flagAndKeep argument utility ------------------------------

  if ( flagAndKeep ) {
    # verb for logging messages
    verb <- "Flagging"

    tbl$rowID <- as.integer(rownames(tbl))

    # duplicate tbl and add columns for flags
    tblFlagged <- tbl
    tblFlagged$QCFlag_anyBad <- FALSE
    tblFlagged$QCFlag_reasonCode <- as.character(NA)
    tblFlagged$QCFlag_badLon <- FALSE
    tblFlagged$QCFlag_badLat <- FALSE
    tblFlagged$QCFlag_badType <- FALSE
    tblFlagged$QCFlag_badFlow <- FALSE
    tblFlagged$QCFlag_badAT <- FALSE
    tblFlagged$QCFlag_badRHi <- FALSE
    tblFlagged$QCFlag_badConcHr <- FALSE

  } else {

    # verb for logging messages
    verb <- "Discarding"

  }

  # ----- Location -------------------------------------------------------------

  # Latitude and longitude must be in range and non-zero
  goodLonMask <-
    !is.na(tbl$Longitude) &
    (tbl$Longitude >= valid_longitude[1]) &
    (tbl$Longitude <= valid_longitude[2]) &
    (tbl$Longitude != 0)

  goodLatMask <-
    !is.na(tbl$Latitude) &
    (tbl$Latitude >= valid_latitude[1]) &
    (tbl$Latitude <= valid_latitude[2]) &
    (tbl$Latitude != 0)


  badRows <- !(goodLonMask & goodLatMask)
  badRowCount <- sum(badRows)

  if ( badRowCount > 0 ) {
    logger.trace("%s %d rows with invalid location information", verb, badRowCount)
    badLocations <- paste('(', tbl$Longitude[badRows], ',', tbl$Latitude[badRows], ')' , sep = '')
    logger.trace("Bad locations: %s", unique(badLocations))
    if ( flagAndKeep ) {
      # apply flags
      tblFlagged$QCFlag_badLon[tbl$rowID[!goodLonMask]] <- TRUE
      tblFlagged$QCFlag_badLat[tbl$rowID[!goodLatMask]] <- TRUE
      tblFlagged$QCFlag_anyBad <- tblFlagged$QCFlag_anyBad | tblFlagged$QCFlag_badLon | tblFlagged$QCFlag_badLat
      # apply reason codes
      tblFlagged$QCFlag_reasonCode[tbl$rowID[!goodLonMask]] <- paste(tblFlagged$QCFlag_reasonCode[tbl$rowID[!goodLonMask]], "badLon")
      tblFlagged$QCFlag_reasonCode[tbl$rowID[!goodLatMask]] <- paste(tblFlagged$QCFlag_reasonCode[tbl$rowID[!goodLatMask]], "badLat")
    }
  }

  tbl <- tbl[goodLonMask & goodLatMask,]

  # Sanity check -- row count
  if (nrow(tbl) < 1 && !flagAndKeep) {
    msg <- sprintf("no valid PM2.5 data")
    logger.warn(msg)    # This is more of a warning than some error in the data.
    stop(msg)
  }

  # ----- QC -------------------------------------------------------------------


  ### Leland Tarnay QC from 2017
  ###
  ### dat.2012arbbamraw$concHR <- ifelse(dat.2012arbbamraw$Qtot.m3.<.834*.95,NA,
  ###                                    ifelse(dat.2012arbbamraw$Qtot.m3.>.834*1.05,NA,
  ###                                    ifelse(dat.2012arbbamraw$IT.C.>45,NA,
  ###                                    ifelse(dat.2012arbbamraw$RH...> 45,NA,
  ###                                    ifelse(dat.2012arbbamraw$Conc.mg.<0,0,
  ###                                    ifelse(dat.2012arbbamraw$Conc.mg.>.984,NA,
  ###                                    ifelse(dat.2012arbbamraw$Delta.C.>25,NA,
  ###                                    dat.2012arbbamraw$Conc.mg.*1000)))))))

  # NOTE:  Override ConcHr high value with 5000 as per conversation with Mike Broughton

  # TODO:  Consider logic to throw out pm25 data if temperature changes by more
  # TODO:  than 2 degC in adjacent hrs.
  # TODO:  (see NOTE in section 2.2 here: https://www.arb.ca.gov/airwebmanual/instrument_manuals/Documents/BAM-1020-9800%20Manual%20Rev%20G.ptbl)

  goodFlow <- !is.na(tbl$flow) & tbl$flow >= valid_flow[1] & tbl$flow <= valid_flow[2]
  goodAT <- !is.na(tbl$AT) & tbl$AT >= valid_AT[1] & tbl$AT <= valid_AT[2]
  goodRHi <- !is.na(tbl$RHi) & tbl$RHi >= valid_RHi[1] & tbl$RHi <= valid_RHi[2]
  goodConcHr <- !is.na(tbl$pm25) & tbl$pm25 >= valid_pm25[1] & tbl$pm25 <= valid_pm25[2]
  gooddatetime <- !is.na(tbl$datetime) & tbl$datetime < lubridate::now(tzone = "UTC") # saw a future date once

  logger.trace("Flow has %s missing or out of range values", sum(!goodFlow))
  if (sum(!goodFlow) > 0) logger.trace("Bad Flow values:  %s", paste0(sort(unique(tbl$flow[!goodFlow]),na.last = TRUE), collapse = ", "))
  logger.trace("AT has %s missing or out of range values", sum(!goodAT))
  if (sum(!goodAT) > 0) logger.trace("Bad AT values:  %s", paste0(sort(unique(tbl$AT[!goodAT]),na.last = TRUE), collapse = ", "))
  logger.trace("RHi has %s missing or out of range values", sum(!goodRHi))
  if (sum(!goodRHi) > 0) logger.trace("Bad RHi values:  %s", paste0(sort(unique(tbl$RHi[!goodRHi]),na.last = TRUE), collapse = ", "))
  logger.trace("Conc has %s missing or out of range values", sum(!goodConcHr))
  if (sum(!goodConcHr) > 0) logger.trace("Bad Conc values:  %s", paste0(sort(unique(tbl$pm25[!goodConcHr]),na.last = TRUE), collapse = ", "))
  logger.trace("datetime has %s missing or out of range values", sum(!gooddatetime))
  if (sum(!gooddatetime) > 0) logger.trace("Bad datetime values:  %s", paste0(sort(unique(tbl$datetime[!gooddatetime]),na.last = TRUE), collapse = ", "))

  goodMask <- goodFlow & goodAT & goodRHi & goodConcHr & gooddatetime
  badQCCount <- sum(!goodMask)

  if ( badQCCount > 0 ) {
    logger.trace(paste(verb,"%s rows because of QC logic"), badQCCount)
    if ( flagAndKeep ) {
      # apply flags
      tblFlagged$QCFlag_badFlow[tbl$rowID[!goodFlow]] <- TRUE
      tblFlagged$QCFlag_badAT[tbl$rowID[!goodAT]] <- TRUE
      tblFlagged$QCFlag_badRHi[tbl$rowID[!goodRHi]] <- TRUE
      tblFlagged$QCFlag_badConcHr[tbl$rowID[!goodConcHr]] <- TRUE
      tblFlagged$QCFlag_badDateAndTime[tbl$rowID[!gooddatetime]] <- TRUE
      tblFlagged$QCFlag_anyBad <- (tblFlagged$QCFlag_anyBad | tblFlagged$QCFlag_badFlow | tblFlagged$QCFlag_badAT |
                                    tblFlagged$QCFlag_badRHi | tblFlagged$QCFlag_badConcHr | tblFlagged$QCFlag_badDateAndTime)
      # apply reason codes
      tblFlagged$QCFlag_reasonCode[tbl$rowID[!goodFlow]] <- paste(tblFlagged$QCFlag_reasonCode[tbl$rowID[!goodFlow]],"badFlow")
      tblFlagged$QCFlag_reasonCode[tbl$rowID[!goodAT]] <- paste(tblFlagged$QCFlag_reasonCode[tbl$rowID[!goodAT]],"badAT")
      tblFlagged$QCFlag_reasonCode[tbl$rowID[!goodRHi]] <- paste(tblFlagged$QCFlag_reasonCode[tbl$rowID[!goodRHi]],"badRHi")
      tblFlagged$QCFlag_reasonCode[tbl$rowID[!goodConcHr]] <- paste(tblFlagged$QCFlag_reasonCode[tbl$rowID[!goodConcHr]],"badConcHr")
      tblFlagged$QCFlag_reasonCode[tbl$rowID[!gooddatetime]] <- paste(tblFlagged$QCFlag_reasonCode[tbl$rowID[!gooddatetime]],"badDateAndTime")
    }
  }

  tbl <- tbl[goodMask,]

  # Sanity check -- row count
  if (nrow(tbl) < 1 && !flagAndKeep) {
    msg <- paste0("No valid PM2.5 data for ", monitorName)
    logger.warn(msg) # This is more of a warning than some error in the data.
    stop(msg, call. = FALSE)
  }

  # ----- More QC -------------------------------------------------------------

  # NOTE:  Additional QC would go here

  if ( flagAndKeep ) {
    logger.trace("Retaining %d rows of measurements; %d bad rows flagged", nrow(tbl), sum(tblFlagged$QCFlag_anyBad))
  } else {
    logger.trace("Retaining %d rows of validated measurements", nrow(tbl))
  }

  # ----- Final cleanup -------------------------------------------------------

  if ( flagAndKeep ) {
    tblFlagged$QCFlag_reasonCode <- stringr::str_sub(tblFlagged$QCFlag_reasonCode, 3)
    tblFlagged$QCFlag_reasonCode <- stringr::str_trim(tblFlagged$QCFlag_reasonCode)
    tbl <- tblFlagged
    tbl$rowID <- NULL
  }

  return(tbl)

}
