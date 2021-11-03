#' @export
#' @importFrom utils read.table
#' @importFrom rlang .data
#' @importFrom dplyr all_of
#'
#' @title Create a 'data' dataframe with monitor data
#'
#' @description
#' Create a \code{data} dataframe with AirNow monitor data appropriate
#' for use with the \pkg{MazamaTimeSeries} package.
#'
#' The data model is that monitor metadata are stored in a tibble named \code{meta}.
#' with a \code{deviceDeploymentID} unique identifier that is matched by column
#' names in an associated \code{data} file.
#'
#' @param meta Table of metadata produced with \code{airnow_createMeta}.
#' @param airnow_data Table of monitor data obtained with \code{airnow_getData()}.
#'
#' @return Tibble of device-deployment metadata.
#'

airnow_createData <- function(
  meta = NULL,
  airnow_data = NULL
) {

  if ( logger.isInitialized() )
    logger.debug(" ----- airnow_createData() ----- ")

  # ----- Validate Parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(meta)
  MazamaCoreUtils::stopIfNull(airnow_data)

  # ----- Prepare data ---------------------------------------------------------

  # NOTE:  We are going to ignore many of the following columns.
  # NOTE:  The only ones we need are those associated with location, time and
  # NOTE:  the actual measurement itself.

  #   > dplyr::glimpse(airnow_data, width = 75)
  #   Rows: 6,746
  #   Columns: 13
  #   $ latitude                  <dbl> 21.32360, 21.39283, 21.30390, 21.31030,
  #   $ longitude                 <dbl> -158.0886, -157.9691, -157.8711, -157.8
  #   $ utcTime                   <dttm> 2021-11-03 11:00:00, 2021-11-03 11:00:
  #   $ parameterName             <chr> "PM2.5", "PM2.5", "PM2.5", "PM2.5", "PM
  #   $ parameterConcentration    <dbl> 0.6, 0.7, 2.0, 0.6, 4.0, 2.0, 10.3, 3.1
  #   $ parameterUnits            <chr> "UG/M3", "UG/M3", "UG/M3", "UG/M3", "UG
  #   $ parameterRawConcentration <dbl> 0.00000, 1.00000, 0.00000, 0.00000, 5.0
  #   $ parameterAQI              <dbl> 3, 3, 8, 3, 17, 8, 43, 13, 53, 33, 13,
  #   $ parameterAQC              <chr> "1", "1", "1", "1", "1", "1", "1", "1",
  #   $ siteName                  <chr> "Kapolei", "Pearl City", "SAND ISLAND 2
  #   $ agencyName                <chr> "Hawaii State Dept. of Health", "Hawaii
  #   $ AQSID                     <chr> "150030010", "150032004", "150031004",
  #   $ fullAQSID                 <chr> "840150030010", "840150032004", "840150

  if ( logger.isInitialized() ) logger.trace("Adding standard data ...")


  airnow_data <-

    airnow_data %>%

    dplyr::rename(
      datetime = .data$utcTime
    ) %>%
    dplyr::mutate(
      locationID = MazamaLocationUtils::table_getLocationID(
        locationTbl,
        .data$longitude,
        .data$latitude,
        distanceThreshold = distanceThreshold
      ),
      deviceID = .data$AQSID
    ) %>%
    dplyr::mutate(
      deviceDeploymentID = paste0(.data$locationID, "_", .data$deviceID)
    )


  # Check that all deviceIDs are found
  missingDeviceIDs <- setdiff(airnow_data$deviceID, meta$deviceID)
  if ( length(missingDeviceIDs) > 0 ) {
    err_msg <-
      sprintf(
        "Devices found in 'airnow_data' that are not found in 'meta' include: %s",
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

  # # Use a named vector to quickly match deviceIDs to deviceDeploymentIDs
  # deviceDeploymentID <- meta$deviceDeploymentID
  # names(deviceDeploymentID) <- meta$deviceID
  #
  # AQS_data$deviceDeploymentID <- deviceDeploymentID[AQS_data$deviceID]
  #
  #
  #
  #






  # ----- Reshape data ---------------------------------------------------------

  rawData <-

    airnow_data %>%

    # Guarantee that we only have one record per hour
    dplyr::select(all_of(c("datetime", "parameterRawConcentration", "deviceDeploymentID"))) %>%
    dplyr::distinct() %>%

    # Reshape to [datetime x deviceDeploymentID]
    reshape2::melt(
      id.vars = c("datetime", "deviceDeploymentID"),
      measure.vars = c("parameterRawConcentration")
    ) %>%
    reshape2::dcast(datetime ~ deviceDeploymentID)







  #'   # Use dplyr and reshape2 packages to seprate the data by parameter and restructure each data frame
  #'   for ( parameter in parameters ) {
  #'
  #'     logger.trace("Reshaping data for %s ...", parameter)
  #'
  #'     # Create datetime variable
  #'     tbl <- dplyr::filter(airnowTbl, airnowTbl$ParameterName == parameter)
  #'     datestamp <- paste0(tbl$ValidDate, ' ', tbl$ValidTime)
  #'     tbl$datetime <- lubridate::mdy_hm(datestamp) # 'mdy_hm', not 'ymd_hm'
  #'     # Guarantee unique rows
  #'     tbl <- dplyr::distinct(tbl)
  #'     # Melt and recast (convert tibbles to dataframes)
  #'     melted <- reshape2::melt(tbl, id.vars=c('datetime','monitorID'), measure.vars=c('Value'))
  #'     dfList[[parameter]] <- reshape2::dcast(melted, datetime ~ monitorID)
  #'
  #'   }
  #'
  #'   # NOTE:  Some parameters, especially those with few monitors, may not have
  #'   # NOTE:  measurements for for every single hour. Here we guarantee that the
  #'   # NOTE:  reshaped dataframes we return will have a row for every single hour
  #'   # NOTE:  in a month, even if that row is filled with NAs.
  #'
  #'   # Guarantee that all times are present by starting with a dataframe containing only a uniform time axis.
  #'   starttime <- MazamaCoreUtils::parseDatetime(startdate, timezone = "UTC")
  #'   timeAxis <- seq(starttime, starttime + lubridate::dhours(hours-1), by = 'hours')
  #'   hourlyDF <- data.frame(datetime=timeAxis)
  #'
  #'   logger.trace("Putting data on a uniform time axis ...")
  #'
  #'   for ( parameter in parameters ) {
  #'
  #'     # Join data to uniform time axis
  #'     dfList[[parameter]] <- suppressMessages({
  #'       dplyr::full_join(hourlyDF, dfList[[parameter]])
  #'     })
  #'
  #'     # NOTE:  Check this URL for some EPA defined levels:
  #'     # NOTE:    https://aqs.epa.gov/aqsweb/documents/codetables/aqi_breakpoints.csv
  #'
  #'     # Assume this data has been QC'ed and let everything through
  #'     dfList[[parameter]] <- airnow_qualityControl(dfList[[parameter]],
  #'                                                  limits = c(-Inf,Inf))
  #'
  #'   }


  # ----- Return ---------------------------------------------------------------

  return(data)

}
