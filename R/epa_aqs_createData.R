#' @export
#' @import MazamaCoreUtils
#'
#' @title Create a 'data' dataframe with required data
#'
#' @description
#' Create a \code{data} dataframe with EPA AQS monitoring data appropriate
#' for use with the \pkg{MazamaTimeSeries} package.
#'
#' The data model is that data are stored in a tibble named \code{data} with an initial
#' \code{datetime} column and monitor-specific data in columns identified with a
#' \code{deviceDeploymentID} that matches one found in the incoming \code{meta} table
#'
#' @param AQS_data Table of AQS monitoring data produced with \pkg{epa_aqs_parseHourlyData()}.
#' @param meta Table of monitor metadata.
#' @param parameterCode EPA "Parameter Code".
#'
#' @return Restructured tibble  with data organized as timeseries columns.
#'

epa_aqs_createData <- function(
  AQS_data = NULL,
  meta = NULL,
  parameterCode = NULL
) {

  if ( logger.isInitialized() )
    logger.debug(" ----- epa_aqs_createData() ----- ")

  # ----- Validate Parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(AQS_data)
  MazamaCoreUtils::stopIfNull(meta)
  MazamaCoreUtils::stopIfNull(parameterCode)

  # ----- Prepare data ---------------------------------------------------------

  # NOTE:  We are going to ignore almost all of the following columns.
  # NOTE:  The only ones we need are those associated with location, time and
  # NOTE:  the actual measurement itself.

  #   > dplyr::glimpse(AQS_data, width = 75)
  #   Rows: 70,043
  #   Columns: 24
  #   $ `State Code`          <chr> "06", "06", "06", "06", "06", "06", "06", "
  #   $ `County Code`         <chr> "019", "019", "019", "019", "019", "019", "
  #   $ `Site Num`            <chr> "5001", "5001", "5001", "5001", "5001", "50
  #   $ `Parameter Code`      <chr> "88101", "88101", "88101", "88101", "88101"
  #   $ POC                   <chr> "3", "3", "3", "3", "3", "3", "3", "3", "3"
  #   $ Latitude              <dbl> 36.81945, 36.81945, 36.81945, 36.81945, 36.
  #   $ Longitude             <dbl> -119.7164, -119.7164, -119.7164, -119.7164,
  #   $ Datum                 <chr> "NAD83", "NAD83", "NAD83", "NAD83", "NAD83"
  #   $ `Parameter Name`      <chr> "PM2.5 - Local Conditions", "PM2.5 - Local
  #   $ `Date Local`          <chr> "2008-11-25", "2008-11-25", "2008-11-25", "
  #   $ `Time Local`          <chr> "16:00", "17:00", "18:00", "19:00", "20:00"
  #   $ `Date GMT`            <chr> "2008-11-26", "2008-11-26", "2008-11-26", "
  #   $ `Time GMT`            <chr> "00:00", "01:00", "02:00", "03:00", "04:00"
  #   $ `Sample Measurement`  <dbl> 64, 62, 50, 50, 63, 72, 77, 64, 73, 73, 69,
  #   $ `Units of Measure`    <chr> "Micrograms/cubic meter (LC)", "Micrograms/
  #   $ MDL                   <dbl> 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5
  #   $ Uncertainty           <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
  #   $ Qualifier             <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
  #   $ `Method Type`         <chr> "FEM", "FEM", "FEM", "FEM", "FEM", "FEM", "
  #   $ `Method Code`         <chr> "170", "170", "170", "170", "170", "170", "
  #   $ `Method Name`         <chr> "Met One BAM-1020 Mass Monitor w/VSCC - Bet
  #   $ `State Name`          <chr> "California", "California", "California", "
  #   $ `County Name`         <chr> "Fresno", "Fresno", "Fresno", "Fresno", "Fr
  #   $ `Date of Last Change` <chr> "2010-09-16", "2010-09-16", "2010-09-16", "

  if ( logger.isInitialized() ) logger.trace("Adding standard data ...")

  # * datetime -----

  # Create a column with the datetime
  timeString <- sprintf("%s %s:00", AQS_data$`Date GMT`, AQS_data$`Time GMT`)
  AQS_data$datetime <- MazamaCoreUtils::parseDatetime(timeString, timezone = "UTC")

  # * deviceID -----

  # Add deviceID to match what is done in epa_createMetaDataframes().
  AQS_data$deviceID <-
    sprintf(
      "%s%s%s_%02d",
      AQS_data$`State Code`, AQS_data$`County Code`, AQS_data$`Site Num`,
      as.numeric(AQS_data$POC)
    )

  # Check that all deviceIDs are found
  missingDeviceIDs <- setdiff(AQS_data$deviceID, meta$deviceID)
  if ( length(missingDeviceIDs) > 0 ) {
    err_msg <-
      sprintf(
        "Devices found in 'AQS_data' that are not found in 'meta' include: %s",
        paste0(missingDeviceIDs, collapse = ", ")
      )
    warning(err_msg)
  }

  # NOTE:  Locations found in the data files have been found to be up to 20+
  # NOTE:  meters off from locations reported for the same State-County-Site-POC
  # NOTE:  in Sites or Monitors tables. But 'deviceID' should match.

  # NOTE:  For AQS_88101_meta:
  # NOTE:    > any(duplicated(meta$deviceID))
  # NOTE:    [1] FALSE

  # NOTE:  We will use 'deviceID' as the monitor unique identifier and obtain
  # NOTE:  the 'deviceDeploymentID' from it.

  # Use a named vector to quickly match deviceIDs to deviceDeploymentIDs
  deviceDeploymentID <- meta$deviceDeploymentID
  names(deviceDeploymentID) <- meta$deviceID

  AQS_data$deviceDeploymentID <- deviceDeploymentID[AQS_data$deviceID]

  # ----- Reshape data ---------------------------------------------------------

  if ( logger.isInitialized() ) logger.trace("Reshape data ...")

  # * melt ----

  if ( logger.isInitialized() ) logger.trace(" * melt")

  # "melt" the data frame into long-format data containing a single measurement
  melted <-
    reshape2::melt(
      data = AQS_data,
      id.vars = c("datetime", "deviceDeploymentID"),
      measure.vars = "Sample Measurement"
    )

  # * validate -----

  if ( logger.isInitialized() ) logger.trace(" * validate")

  # Should only have one pm25DF measure per hour
  valueCountPerCell <-
    reshape2::dcast(melted, datetime ~ deviceDeploymentID, length)

  maxCount <- max(valueCountPerCell[,-1])

  if ( (maxCount > 1) && logger.isInitialized() ) {
    logger.warn("Up to %s measurements per hour -- median used", maxCount)
  }

  # * reshape -----

  if ( logger.isInitialized() ) logger.trace(" * reshape")

  # Create a tibble for data values
  parameterDF <- reshape2::dcast(melted, datetime ~ deviceDeploymentID, stats::median)

  # * regular time axis -----

  if ( logger.isInitialized() ) logger.trace(" * regular time axis")

  # NOTE:  We want to guarantee that there is a record for every single hour
  # NOTE:  even if no data are available in that hour.

  # Create a dataframe for hours
  hourlyDF <- data.frame(seq(min(melted$datetime), max(melted$datetime), by = "hours"))
  names(hourlyDF) <- "datetime"

  # Merge the two dataframes together with a left join
  data <- dplyr::left_join(hourlyDF, parameterDF, by = "datetime")

  if ( logger.isInitialized() )
    logger.trace("'data' dataframe has %d rows and %d columns", nrow(data), ncol(data))

  # ----- Return ---------------------------------------------------------------

  return(data)

}

# ===== DEBUGGING ==============================================================

if ( FALSE ) {

  AQS_data <- epa_aqs_parseHourlyData("~/Data/EPA/hourly_88101_2008.zip")
  meta <- get(load("~/Data/EPA/AQS_88101_meta.rda"))
  parameterCode <- "88101"

}

