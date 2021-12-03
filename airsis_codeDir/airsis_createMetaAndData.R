#' @export
#' @importFrom utils read.table
#' @importFrom rlang .data
#'
#' @title Create 'meta' dnad 'data' dataframes from airsis_ data
#'
#' @description
#' Create a \code{meta} dataframe with AIRSIS monitor metadata and a \code{data}
#' dataframe with PM2.5 time series appropriate for use with the
#' \pkg{MazamaTimeSeries} package.
#'
#' The data model has monitor metadata stored in a tibble named \code{meta}.
#' with a \code{deviceDeploymentID} unique identifier that is matched by column
#' names in an associated \code{data} file.
#'
#' @param locationTbl Tibble of "known locations" produced with \pkg{MazamaLocationUtils}.
#' @param distanceThreshold Separation distance in meters between "known locations".
#' @param airsis_data Tibble of AIRSIS monitor data after QC and clustering have
#' been applied.
#'
#' @return List with two tibbles.
#'

# NOTE:  We need to create both 'meta' and 'data' in a single function so that
# NOTE:  we can take advantage of the clustering we attach to airsis_data when
# NOTE:  creating the 'data' dataframe.

airsis_createMetaAndData <- function(
  locationTbl = NULL,
  distanceThreshold = NULL,
  airsis_data = NULL
) {

  logger.debug(" ----- airsis_createMeta() ----- ")

  # ----- Validate Parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(locationTbl)
  MazamaCoreUtils::stopIfNull(distanceThreshold)
  MazamaCoreUtils::stopIfNull(airsis_data)

  # ----- Simplify airsis_data -------------------------------------------------

  # > dplyr::glimpse(airsis_data, width = 75)
  # Rows: 190
  # Columns: 14
  # $ datetime          <dttm> 2013-05-22 22:00:00, 2013-05-22 23:00:00, 2013
  # $ longitude         <dbl> -119.7840, -119.7840, -119.7840, -119.7840, -11
  # $ latitude          <dbl> 37.67461, 37.67461, 37.67461, 37.67461, 37.6746
  # $ flow              <dbl> 0.834, 0.834, 0.834, 0.834, 0.834, 0.834, 0.834
  # $ AT                <dbl> 19.5, 19.5, 18.9, 17.6, 15.9, 12.8, 10.7, 9.7,
  # $ RHi               <dbl> 13, 9, 8, 8, 9, 12, 13, 15, 17, 19, 20, 22, 23,
  # $ pm25              <dbl> 1, 4, 6, 3, 4, 2, -2, 1, -1, 0, 4, 3, 2, 3, 2,
  # $ airsis_Alias      <chr> "NPS YOS1001 Bam", "NPS YOS1001 Bam", "NPS YOS1
  # $ airsis_dataFormat <chr> "BAM.1020", "BAM.1020", "BAM.1020", "BAM.1020",
  # $ clusterLon        <dbl> -119.784, -119.784, -119.784, -119.784, -119.78
  # $ clusterLat        <dbl> 37.67459, 37.67459, 37.67459, 37.67459, 37.6745
  # $ clusterID         <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  # $ airsis_provider   <chr> "apcd", "apcd", "apcd", "apcd", "apcd", "apcd",
  # $ airsis_unitID     <dbl> 1069, 1069, 1069, 1069, 1069, 1069, 1069, 1069,

  # Only keep lon, lat and device metadata columns
  usefulColumns <- c(
    "longitude",
    "latitude",
    "clusterID",
    "airsis_Alias",
    "airsis_dataFormat",
    "airsis_provider",
    "airsis_unitID"
  )

  airsis_location_data <-

    airsis_data %>%

    dplyr::distinct(.data$clusterID, .keep_all = TRUE) %>%

    dplyr::select(dplyr::all_of(usefulColumns))


  # ----- Find nearest known locations -----------------------------------------

  airsis_data_locations <-
    MazamaLocationUtils::table_getNearestLocation(
      locationTbl,
      airsis_location_data$longitude,
      airsis_location_data$latitude,
      distanceThreshold = distanceThreshold
    )

  # NOTE:  Assume that all the work has been done to update the incoming
  # NOTE:  locationTbl so that all locations in airsis_data are "known".
  # NOTE:
  # NOTE:  Any that are not will be removed with a warning message.

  if ( anyNA(airsis_data_locations$locationID) ) {

    err_msg <- sprintf(
      "%d locations are still unknown and will be removed",
      sum(is.na(airsis_data_locations$locationID))
    )
    if ( logger.isInitialized() ) logger.warn(err_msg)
    warning(err_msg)

    # Retain only truly "known" locations
    mask <- !is.na(airsis_data_locations$locationID)
    airsis_data_locations <- airsis_data_locations[mask,]

  }

  # ----- Create 'meta' --------------------------------------------------------

  # > print(AirMonitor::coreMetadataNames, width = 75)
  #  [1] "deviceDeploymentID"    "deviceID"
  #  [3] "deviceType"            "deviceDescription"
  #  [5] "deviceExtra"           "pollutant"
  #  [7] "units"                 "dataIngestSource"
  #  [9] "dataIngestURL"         "dataIngestUnitID"
  # [11] "dataIngestExtra"       "dataIngestDescription"
  # [13] "locationID"            "locationName"
  # [15] "longitude"             "latitude"
  # [17] "elevation"             "countryCode"
  # [19] "stateCode"             "countyName"
  # [21] "timezone"              "houseNumber"
  # [23] "street"                "city"
  # [25] "zip"                   "AQSID"

  bindColumns <- c(
    "clusterID",
    "airsis_Alias",
    "airsis_dataFormat",
    "airsis_provider",
    "airsis_unitID"
  )

  meta <-

    airsis_data_locations %>%

    # Add device metadata
    dplyr::bind_cols(
      dplyr::select(airsis_location_data, dplyr::all_of(bindColumns))
    ) %>%

    # Unique device ID = AQSID as we have nothing more specific
    dplyr::mutate(
      deviceID = sprintf("%s.%s", .data$airsis_provider, .data$airsis_unitID)
    ) %>%

    # Unique "device deployment" ID
    dplyr::mutate(
      deviceDeploymentID = paste(.data$locationID, .data$deviceID, sep = "_")
    ) %>%

    # Other required metadata
    dplyr::mutate(
      deviceType = .data$airsis_dataFormat,
      deviceDescription = as.character(NA),
      deviceExtra = as.character(NA),
      pollutant = "PM2.5",
      units = "UG/M3",
      dataIngestSource = "AIRSIS",
      dataIngestURL = sprintf("http://%s.airsis.com/vision/common/CSVExport.aspx", .data$airsis_provider),
      dataIngestUnitID = .data$airsis_unitID,
      dataIngestExtra = as.character(NA),
      dataIngestDescription = as.character(NA)
    )

  # NOTE:  Default location names are created from the locationID. When this has
  # NOTE:  *not* been updated in the "known locations" file, we should use the Alias.
  # NOTE:  Here's what it might look like:
  # NOTE:    $ locationID            <chr> "906b5197e3a070e4"
  # NOTE:    $ locationName          <chr> "us.ca_906b51"

  no_assigned_name_mask <-
    stringr::str_sub(meta$locationName, 7, 12) == stringr::str_sub(meta$locationID, 1, 6)

  if ( sum(no_assigned_name_mask) > 0 )
    meta$locationName[no_assigned_name_mask] <- meta$airsis_Alias[no_assigned_name_mask]

  # ----- Reorder 'meta' columns -----------------------------------------------

  coreNames <- AirMonitor::coreMetadataNames
  missingCoreNames <- setdiff(coreNames, names(meta))
  airsisNames <- setdiff(names(meta), coreNames)

  for ( name in missingCoreNames ) {
    meta[[name]] <- as.character(NA)
  }

  meta <-
    meta %>%
    dplyr::select(dplyr::all_of(c(coreNames, airsisNames)))

  # ----- Create hourly axis ---------------------------------------------------

  # NOTE:  We want to guarantee that there is a record for every single hour
  # NOTE:  even if no data are available in that hour.

  # Create a tibble with a regular time axis
  hourlyTbl <- dplyr::tibble(
    datetime = seq(
      min(airsis_data$datetime, na.rm = TRUE),
      max(airsis_data$datetime, na.rm = TRUE),
      by = "hours")
  )

  # ----- Create 'data' --------------------------------------------------------

  # Create a dataframe for reshaping
  airsis_data_enhanced <-

    airsis_data %>%

    # Add deviceDeploymentID
    dplyr::left_join(
      dplyr::select(meta, dplyr::all_of(c("clusterID", "deviceDeploymentID"))),
      by = "clusterID"
    ) %>%

    # Pull out columns for reshaping
    dplyr::select(dplyr::all_of(c("datetime", "pm25", "deviceDeploymentID"))) %>%

    # Use "later is better" logic to get one value per hour
    dplyr::arrange(dplyr::desc(.data$datetime)) %>%
    dplyr::distinct(.data$datetime, .keep_all  = TRUE) %>%
    dplyr::arrange(.data$datetime)

  # Reshape
  melted <- reshape2::melt(
    airsis_data_enhanced,
    id.vars = c("datetime", "deviceDeploymentID"),
    measure.vars = "pm25"
  )

  data <- reshape2::dcast(melted, datetime ~ deviceDeploymentID, stats::median)

  # Merge the two dataframes together with a left join
  data <- dplyr::left_join(hourlyTbl, data, by = "datetime")

  # ----- Clean up -------------------------------------------------------------

  # Remove clusterID
  meta$clusterID <- NULL

  # ----- Return ---------------------------------------------------------------

  dataList = list(
    meta = meta,
    data = data
  )

  return(dataList)

}

# ===== DEBUGGING ==============================================================

if ( FALSE ) {


  library(MazamaCoreUtils)
  logger.setLevel(TRACE)

  library(MazamaSpatialUtils)
  MazamaSpatialUtils::setSpatialDataDir("~/Data/Spatial")
  MazamaSpatialUtils::loadSpatialData("EEZCountries.rda")
  MazamaSpatialUtils::loadSpatialData("OSMTimezones.rda")
  MazamaSpatialUtils::loadSpatialData("NaturalEarthAdm1.rda")
  MazamaSpatialUtils::loadSpatialData("USCensusCounties.rda")

  library(MazamaLocationUtils)
  setLocationDataDir("~/Data/known_locations")
  airsis_locationTbl <- table_initialize()
  airnow_locationTbl <- table_load("airnow_PM2.5_sites")

  library(AirMonitorIngest)

  distanceThreshold <- 1000

  airsis_data <-

    airsis_downloadData(
      startdate = MazamaCoreUtils::parseDatetime("2013-05-20", timezone = "UTC"),
      enddate = MazamaCoreUtils::parseDatetime("2013-05-30", timezone = "UTC"),
      timezone = "UTC",
      provider = "APCD",
      unitID = "1012"
    ) %>%

    airsis_parseData(
      codeDir = "airsis_codeDir"
    ) %>%

    airsis_QC_BAM.1020(
      flagAndKeep = FALSE
    ) %>%

    addClustering(
      clusterDiameter = distanceThreshold,
      lonVar = "longitude",
      latVar = "latitude",
      maxClusters = 50,
      flagAndKeep = FALSE
    )


  locationTbl <- airsis_updateKnownLocations(
    airsis_locationTbl = airsis_locationTbl,
    airnow_locationTbl = airnow_locationTbl,
    distanceThreshold = distanceThreshold,
    airsis_data = airsis_data
  )

}
