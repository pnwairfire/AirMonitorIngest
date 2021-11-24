#' @export
#' @importFrom utils read.table
#' @importFrom rlang .data
#'
#' @title Create a 'meta' dataframe with required monitor metadata
#'
#' @description
#' Create a \code{meta} dataframe with WRCC monitor metadata appropriate
#' for use with the \pkg{MazamaTimeSeries} package.
#'
#' The data model has monitor metadata stored in a tibble named \code{meta}.
#' with a \code{deviceDeploymentID} unique identifier that is matched by column
#' names in an associated \code{data} file.
#'
#' @param locationTbl Table of "known locations" produced with \pkg{MazamaLocationUtils}.
#' @param distanceThreshold Separation distance in meters between "known locations".
#' @param wrcc_data Table of monitor data obtained with \code{epa_api_getData()}.
#' @param unitID WRCC station identifier (will be upcased).
#'
#' @return Tibble of device-deployment metadata.
#'

wrcc_createMeta <- function(
  locationTbl = NULL,
  distanceThreshold = NULL,
  wrcc_data = NULL,
  unitID = NULL
) {

  logger.debug(" ----- wrcc_createMeta() ----- ")

  # ----- Validate Parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(locationTbl)
  MazamaCoreUtils::stopIfNull(distanceThreshold)
  MazamaCoreUtils::stopIfNull(wrcc_data)
  MazamaCoreUtils::stopIfNull(unitID)

  # ----- Simplify wrcc_data -------------------------------------------------

  #   > print(names(wrcc_data), width = 75)
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
    "wrcc_type",
    "wrcc_serialNumber",
    "wrcc_monitorName",
    "wrcc_monitorType"
  )

  wrcc_data <-

    wrcc_data %>%

    dplyr::distinct(.data$clusterID, .keep_all = TRUE) %>%
    dplyr::rename(
      longitude = .data$clusterLon,
      latitude = .data$clusterLat,
      wrcc_type = .data$Type,
      wrcc_serialNumber = .data$SerialNumber,
      wrcc_monitorName = .data$monitorName,
      wrcc_monitorType = .data$monitorType
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

  wrcc_data_locations <-
    MazamaLocationUtils::table_getNearestLocation(
      locationTbl,
      wrcc_data$longitude,
      wrcc_data$latitude,
      distanceThreshold = distanceThreshold
    )

  # NOTE:  Assume that all the work has been done to update the incoming
  # NOTE:  locationTbl so that all locations in wrcc_data are "known".
  # NOTE:
  # NOTE:  Any that are not will be removed with a warning message.

  if ( anyNA(wrcc_data_locations$locationID) ) {

    err_msg <- sprintf(
      "%d locations are still unknown and will be removed",
      sum(is.na(wrcc_data_locations$locationID))
    )
    if ( logger.isInitialized() ) logger.warn(err_msg)
    warning(err_msg)

    # Retain only truly "known" locations
    mask <- !is.na(wrcc_data_locations$locationID)
    wrcc_data_locations <- wrcc_data_locations[mask,]

  }

  # ----- Create 'meta' --------------------------------------------------------

  meta <-

    wrcc_data_locations %>%

    # Add device metadata
    dplyr::bind_cols(
      dplyr::select(wrcc_data, dplyr::starts_with("wrcc_"))
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
      deviceType = .data$wrcc_monitorType,
      deviceDescription = as.character(NA),
      deviceExtra = as.character(NA),
      pollutant = "PM2.5",
      units = "UG/M3",
      dataIngestSource = "WRCC",
      dataIngestURL = "https://wrcc.dri.edu/cgi-bin/wea_list2.pl",
      dataIngestUnitID = unitID,
      dataIngestExtra = as.character(NA),
      dataIngestDescription = as.character(NA)
    )

  # ----- Reorder columns ------------------------------------------------------

  coreNames <- AirMonitor::coreMetadataNames
  missingCoreNames <- setdiff(coreNames, names(meta))
  wrccNames <- setdiff(names(meta), coreNames)

  for ( name in missingCoreNames ) {
    meta[[name]] <- as.character(NA)
  }

  meta <-
    meta %>%
    dplyr::select(dplyr::all_of(c(coreNames, wrccNames)))

  # ----- Return ---------------------------------------------------------------

  return(meta)

}

# ===== DEBUGGING ==============================================================

if ( FALSE ) {


  # See wrcc_updateKnownLocations

}
