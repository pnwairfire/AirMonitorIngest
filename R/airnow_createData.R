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
#' @param locationTbl Table of "known locations" produced with \pkg{MazamaLocationUtils}.
#' @param distanceThreshold Separation distance in meters between "known locations".
#' @param meta Table of metadata produced with \code{airnow_createMeta}.
#' @param airnow_data Table of monitor data obtained with \code{airnow_getData()}.
#'
#' @return Tibble of device-deployment metadata.
#'

airnow_createData <- function(
  locationTbl = NULL,
  distanceThreshold = NULL,
  meta = NULL,
  airnow_data = NULL
) {

  if ( logger.isInitialized() )
    logger.debug(" ----- airnow_createData() ----- ")

  # ----- Validate Parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(locationTbl)
  MazamaCoreUtils::stopIfNull(distanceThreshold)
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

  # ----- Create hourly axis ---------------------------------------------------

  # NOTE:  We want to guarantee that there is a record for every single hour
  # NOTE:  even if no data are available in that hour.

  # Create a tibble with a regular time axis
  hourlyTbl <- dplyr::tibble(
    datetime = seq(
      min(airnow_data$datetime, na.rm = TRUE),
      max(airnow_data$datetime, na.rm = TRUE),
      by = "hours")
  )

  # ----- Reshape raw data -----------------------------------------------------

  rawData <-

    airnow_data %>%

    # Select the columns we need
    dplyr::select(all_of(c("datetime", "parameterRawConcentration", "deviceDeploymentID"))) %>%

    # Guarantee that we only have one record per hour
    dplyr::distinct(.data$datetime, .data$deviceDeploymentID, .keep_all = TRUE) %>%

    # Reshape to [datetime x deviceDeploymentID]
    reshape2::melt(
      id.vars = c("datetime", "deviceDeploymentID"),
      measure.vars = c("parameterRawConcentration")
    ) %>%
    reshape2::dcast(datetime ~ deviceDeploymentID)

  # > setdiff(names(rawData), meta$deviceDeploymentID)
  # [1] "datetime"

  # Merge the two dataframes together with a left join
  rawData <- dplyr::left_join(hourlyTbl, rawData, by = "datetime") %>%
    # Order columns to reflect meta
    dplyr::select(all_of(c('datetime', meta$deviceDeploymentID)))

  if ( logger.isInitialized() )
    logger.trace("rawData' has %d rows and %d columns", nrow(rawData), ncol(rawData))

  # > all(names(rawData) == c('datetime', meta$deviceDeploymentID))
  # [1] TRUE

  # ----- Reshape NowCast data -------------------------------------------------

  nowcastData <-

    airnow_data %>%

    # NOTE:  Data returned by airnow_getData() can have multiple parameterConcentration
    # NOTE:  records per hour. It appears that the valid one will also typically
    # NOTE:  (but not always) have an associated AQI value.
    # NOTE:
    # NOTE:  To guarantee that we only get the preferred record for each hour,
    # NOTE:  We arrange() by parameterAQI which will put the NA values last so
    # NOTE:  that distinct() will get the value associated with a valid
    # NOTE:  parameterAQI if it exists.

    dplyr::arrange(.data$parameterAQI) %>%

    # Select the columns we need
    dplyr::select(all_of(c("datetime", "parameterConcentration", "deviceDeploymentID"))) %>%

    # Guarantee that we only have one record per hour
    dplyr::distinct(.data$datetime, .data$deviceDeploymentID, .keep_all = TRUE) %>%

    # Reshape to [datetime x deviceDeploymentID]
    reshape2::melt(
      id.vars = c("datetime", "deviceDeploymentID"),
      measure.vars = c("parameterConcentration")
    ) %>%
    reshape2::dcast(datetime ~ deviceDeploymentID)

  # > setdiff(names(nowcastData), meta$deviceDeploymentID)
  # [1] "datetime"

  # Merge the two dataframes together with a left join
  nowcastData <-
    dplyr::left_join(hourlyTbl, nowcastData, by = "datetime") %>%
    # Order columns to reflect meta
    dplyr::select(all_of(c('datetime', meta$deviceDeploymentID)))

  if ( logger.isInitialized() )
    logger.trace("'nowcastData' has %d rows and %d columns", nrow(nowcastData), ncol(nowcastData))

  # > all(names(nowcastData) == c('datetime', meta$deviceDeploymentID))
  # [1] TRUE

  # ----- Return ---------------------------------------------------------------

  dataList = list(
    raw = rawData,
    nowcast = nowcastData
  )

  return(dataList)

}
