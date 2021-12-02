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
#' @param unitID AIRSIS station identifier (will be upcased).
#'
#' @return List with two tibbles.
#'

# NOTE:  We need to create both 'meta' and 'data' in a single function so that
# NOTE:  we can take advantage of the clustering we attach to airsis_data when
# NOTE:  creating the 'data' dataframe.

airsis_createMetaAndData <- function(
  locationTbl = NULL,
  distanceThreshold = NULL,
  airsis_data = NULL,
  unitID = NULL
) {

  logger.debug(" ----- airsis_createMeta() ----- ")

  # ----- Validate Parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(locationTbl)
  MazamaCoreUtils::stopIfNull(distanceThreshold)
  MazamaCoreUtils::stopIfNull(airsis_data)
  MazamaCoreUtils::stopIfNull(unitID)

  # ----- Simplify airsis_data -------------------------------------------------

  #   > print(names(airsis_data), width = 75)
  #  [1] "DateTime"       "GPSLat"         "GPSLon"         "Type"
  #  [5] "SerialNumber"   "ConcRT"         "Conc_l_m"       "AvAirFlw"
  #  [9] "AvAirTemp"      "RelHumidity"    "BaromPress"     "SensorIntAT"
  # [13] "SensorIntRH"    "WindSpeed"      "WindDir"        "BatteryVoltage"
  # [17] "Alarm"          "monitorName"    "monitorType"    "datetime"
  # [21] "clusterLon"     "clusterLat"     "clusterID"

  # Only keep lon, lat and device metadata columns
  usefulColumns <- c(
    "longitude",
    "latitude",
    "clusterID",
    "airsis_type",
    "airsis_serialNumber",
    "airsis_monitorName",
    "airsis_monitorType"
  )

  airsis_location_data <-

    airsis_data %>%

    dplyr::distinct(.data$clusterID, .keep_all = TRUE) %>%
    dplyr::rename(
      longitude = .data$clusterLon,
      latitude = .data$clusterLat,
      airsis_type = .data$Type,
      airsis_serialNumber = .data$SerialNumber,
      airsis_monitorName = .data$monitorName,
      airsis_monitorType = .data$monitorType
    ) %>%

    # Remove records with missing or zero lon/lat
    dplyr::filter(
      is.finite(.data$longitude),
      is.finite(.data$latitude),
      .data$longitude != 0,
      .data$latitude != 0
    ) %>%

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

  bindColumns <- c(
    "clusterID",
    "airsis_type",
    "airsis_serialNumber",
    "airsis_monitorName",
    "airsis_monitorType"
  )

  meta <-

    airsis_data_locations %>%

    # Add device metadata
    dplyr::bind_cols(
      dplyr::select(airsis_location_data, dplyr::all_of(bindColumns))
    ) %>%

    # Unique device ID = AQSID as we have nothing more specific
    dplyr::mutate(
      deviceID = paste0("wrcc.", unitID)
    ) %>%

    # Unique "device deployment" ID
    dplyr::mutate(
      deviceDeploymentID = paste(.data$locationID, .data$deviceID, sep = "_")
    ) %>%

    # Other required metadata
    dplyr::mutate(
      deviceType = .data$airsis_monitorType,
      deviceDescription = as.character(NA),
      deviceExtra = as.character(NA),
      pollutant = "PM2.5",
      units = "UG/M3",
      dataIngestSource = "AIRSIS",
      dataIngestURL = "https://wrcc.dri.edu/cgi-bin/wea_list2.pl",
      dataIngestUnitID = unitID,
      dataIngestExtra = as.character(NA),
      dataIngestDescription = as.character(NA)
    )

  # ----- Reorder 'meta' columns -----------------------------------------------

  coreNames <- AirMonitor::coreMetadataNames
  missingCoreNames <- setdiff(coreNames, names(meta))
  wrccNames <- setdiff(names(meta), coreNames)

  for ( name in missingCoreNames ) {
    meta[[name]] <- as.character(NA)
  }

  meta <-
    meta %>%
    dplyr::select(dplyr::all_of(c(coreNames, wrccNames)))

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
    dplyr::select(dplyr::all_of(c("datetime", "ConcRT", "deviceDeploymentID"))) %>%

    # Use "later is better" logic to get one value per hour
    dplyr::arrange(dplyr::desc(.data$datetime)) %>%
    dplyr::distinct(.data$datetime, .keep_all  = TRUE) %>%
    dplyr::arrange(.data$datetime)

  # Reshape
  melted <- reshape2::melt(
    airsis_data_enhanced,
    id.vars = c("datetime", "deviceDeploymentID"),
    measure.vars = "ConcRT"
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


  # See airsis_updateKnownLocations

}
