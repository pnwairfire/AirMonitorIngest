#' @export
#' @importFrom utils read.table
#' @importFrom rlang .data
#'
#' @title Create a 'meta' dataframe with required monitor metadata
#'
#' @description
#' Create a \code{meta} dataframe with AirNow monitor metadata appropriate
#' for use with the \pkg{MazamaTimeSeries} package.
#'
#' The data model is that monitor metadata are stored in a tibble named \code{meta}.
#' with a \code{deviceDeploymentID} unique identifier that is matched by column
#' names in an associated \code{data} file.
#'
#' @param locationTbl Table of "known locations" produced with \pkg{MazamaLocationUtils}.
#' @param distanceThreshold Separation distance in meters between "known locations".
#' @param airnow_data Table of monitor data obtained with \code{epa_api_getData()}.
#'
#' @return Tibble of device-deployment metadata.
#'

airnow_createMeta <- function(
  locationTbl = NULL,
  distanceThreshold = NULL,
  airnow_data = NULL
) {

  if ( logger.isInitialized() )
    logger.debug(" ----- airnow_createMeta() ----- ")

  # ----- Validate Parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(locationTbl)
  MazamaCoreUtils::stopIfNull(distanceThreshold)
  MazamaCoreUtils::stopIfNull(airnow_data)

  unitsTable <- table(airnow_data$parameterUnits)[1]
  if ( length(unitsTable) > 1 ) {
    err_msg <- sprintf(
      "multiple units found: %s",
      paste0(names(unitsTable), collapse = ", ")
    )
  }
  units <- names(unitsTable)[1]

  # ----- Simplify airnow_data -------------------------------------------------

  airnow_data <-

    airnow_data %>%

    # Saw some AQSID with two records per hour, one with and one without parameterAQI
    dplyr::arrange(.data$parameterAQI) %>%
    dplyr::distinct(.data$AQSID, .keep_all = TRUE) %>%

    # Remove records with missing or zero lon/lat
    dplyr::filter(
      is.finite(.data$longitude),
      is.finite(.data$latitude),
      .data$longitude != 0,
      .data$latitude != 0
    )

  # ----- Find nearest known locations -----------------------------------------

  airnow_data_locations <-
    MazamaLocationUtils::table_getNearestLocation(
      locationTbl,
      airnow_data$longitude,
      airnow_data$latitude,
      distanceThreshold = distanceThreshold
    )

  # NOTE:  Assume that all the work has been done to update the incoming
  # NOTE:  locationTbl so that all locations in airnow_data are "known".
  # NOTE:
  # NOTE:  Any that are not will be removed with a warning message.

  if ( anyNA(airnow_data_locations$locationID) ) {

    err_msg <- sprintf(
      "%d locations are still unknown and will be removed",
      length(is.na(airnow_data_locations$locationID))
    )
    if ( logger.isInitialized() ) logger.warn(err_msg)
    warning(err_msg)

    # Retain only truly "known" locations
    mask <- !is.na(.data$locationID)
    airnow_data_locations <- airnow_data_locations[mask,]

    # NOTE:  At this point, there may be duplicate locationIDs associated with
    # NOTE:  different AQSIDs in airnow_data. Make sure we are using the latest
    # NOTE:  AQSIDs from airnow_data.

    # Add AQSID
    airnow_data_locations$AQSID <- airnow_data$AQSID[mask]

  }

  # ----- Create 'meta' --------------------------------------------------------

  meta <-

    airnow_data_locations %>%

    # Unique device ID = AQSID as we have nothing more specific
    dplyr::mutate(
      deviceID = .data$AQSID
    ) %>%

    # Unique "device deployment" ID
    dplyr::mutate(
      deviceDeploymentID = paste(.data$locationID, .data$deviceID, sep = "_")
    ) %>%

    # Other required metadata
    dplyr::mutate(
      deviceType = as.character(NA),
      deviceDescription = as.character(NA),
      deviceExtra = as.character(NA),
      pollutant = .data$airnow_parameterName,
      units = !!units,
      dataIngestSource = "AirNow",
      dataIngestURL = "https://www.airnowapi.org/aq/data/",
      dataIngestUnitID = as.character(NA),
      dataIngestExtra = as.character(NA),
      dataIngestDescription = as.character(NA)
    ) %>%

    # NOTE:  The use of table_getNearestLocation() above may have assigned
    # NOTE:  multiple records from airnow_data with marginally different
    # NOTE:  longitude or latitude values to the same locationID. This is
    # NOTE:  exactly as designed. (We have seen mobile monitors behind the
    # NOTE:  CARB office in Sacramento reporting slightly different locations.)
    # NOTE:
    # NOTE:  Because of this, we need to filter out any records with duplicate
    # NOTE:  deviceDeploymentIDs.

    # Guarantee unique deviceDeploymentIDs
    dplyr::distinct(.data$deviceDeploymentID, .keep_all = TRUE)

  # ----- Reorder columns ------------------------------------------------------

  coreNames <- AirMonitor::coreMetadataNames
  missingCoreNames <- setdiff(coreNames, names(meta))
  airnowNames <- setdiff(names(meta), coreNames)

  for ( name in missingCoreNames ) {
    meta[[name]] <- as.character(NA)
  }

  meta <-
    meta %>%
    dplyr::select(dplyr::all_of(c(coreNames, airnowNames)))

  # ----- Return ---------------------------------------------------------------

  return(meta)

}

# ===== DEBUGGING ==============================================================

if ( FALSE ) {


  library(MazamaCoreUtils)
  logger.setLevel(TRACE)

  library(AirMonitorIngest)
  setAPIKey("airnow", Sys.getenv("AIRNOW_API_KEY"))


  MazamaLocationUtils::setLocationDataDir("~/Data/known_locations")
  locationTbl <- MazamaLocationUtils::table_load("airnow_PM2.5_sites")

  distanceThreshold <- 100

  starttime <- 2021102700
  endtime <- 2021102700
  timezone <- "America/Los_Angeles"
  parameterName <- "PM2.5"
  monitorType <- "both"

  airnow_data <-
    airnow_getData(
      parameterName = parameterName,
      starttime = starttime,
      endtime = endtime,
      timezone = timezone,
      monitorType = monitorType
    )




}
