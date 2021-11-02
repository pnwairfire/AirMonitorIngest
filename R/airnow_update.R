#' @export
#'
#' @title Update AirNow data
#'
#' @param collectionName Character identifier for this table.
#' @param distanceThreshold Separation distance in meters between "known locations".
#' @param parameterName An EPA AQS criteria parameter name.
#'
#' @description
#' Download AirNow sites metadata and monitoring data, update the known locations
#' table and return a list with the "meta" and "data" tables filled with recent
#' data.
#'

airnow_update <- function(
  collectionName = NULL,
  distanceThreshold = 100,
  parameterName = c("PM2.5") ###, "CO", "OZONE", "PM10"),
) {

  if ( logger.isInitialized() )
    logger.debug(" ----- airnow_update() ----- ")

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(collectionName)
  MazamaCoreUtils::stopIfNull(distanceThreshold)

  parameterName <- match.arg(parameterName)

  # ----- Load known locations -------------------------------------------------

  locationDataDir <- MazamaLocationUtils::getLocationDataDir()

  MazamaCoreUtils::stopIfNull(
    locationDataDir,
    "locationDataDir is not set. Set it with MazamaLocationUtils::setLocationDataDir()."
  )

  locationsFilePath <- file.path(locationDataDir, paste0(collectionName, ".rda"))

  if ( file.exists(locationsFilePath) ) {
    locationTbl <- MazamaLocationUtils::table_load(collectionName)
  } else {
    locationTbl <- MazamaLocationUtils::table_initialize()
  }

  # ----- Update known locations -----------------------------------------------

  locationTbl <-
    airnow_updateKnownLocations(
      locationTbl = locationTbl,
      distanceThreshold = distanceThreshold,
      parameterName = parameterName
    )

  # NOTE:  locationTbl now has all locations found in AirNow's sites metadata file

  # ----- Download data --------------------------------------------------------

  # Default to the last 6+ hours of data
  endtime <- lubridate::now(tzone = "UTC")
  starttime <- lubridate::floor_date(endtime, unit = "hours") - lubridate::dhours(5)
  timezone <- "UTC"

  airnow_data <-
    airnow_getData(
      starttime = starttime,
      endtime = endtime,
      timezone = timezone,
      parameterName = parameterName,
      monitorType = "both"
    )

  # ----- Update unknown locations ---------------------------------------------

  # NOTE:  Unknown locations are those found in airnow_data but not in airnow_sites.
  # NOTE:  We look for known locations in locationTbl and then split off an
  # NOTE:  records in airnow_data that have no known location. For these, we
  # NOTE:  add minimalist records to locationTbl.
  # NOTE:
  # NOTE:  At that point, every location in airnow_data will be represented by
  # NOTE:  a "known location" in locationTbl.

  # Find individual locations assuming last-is-best
  airnow_data_locations <-
    airnow_data %>%
    dplyr::arrange(dplyr::desc(.data$utcTime)) %>%
    dplyr::distinct(.data$longitude, .data$latitude, .keep_all = TRUE)

  # Add locationID
  airnow_data_locations$locationID <-
    MazamaLocationUtils::table_getLocationID(
      locationTbl = locationTbl,
      longitude = airnow_data_locations$longitude,
      latitude = airnow_data_locations$latitude,
      distanceThreshold = distanceThreshold,
      measure = "geodesic"
    )

  # Split known/unknown
  airnow_known <-
    airnow_data_locations %>%
    dplyr::filter(!is.na(.data$locationID))

  airnow_unknown <-
    airnow_data_locations %>%
    dplyr::filter(is.na(.data$locationID))

  # Add records for the "unknown" locations
  locationTbl <-
    airnow_updateUnknownLocations(
      locationTbl,
      airnow_unknown,
      distanceThreshold
    )

  # NOTE:  locationTbl now has records for data NOT found in AirNow's sites metadata file

  # ----- Create 'meta' --------------------------------------------------------

  meta <-
    airnow_createMeta(
      locationTbl,
      airnow_data,
      distanceThreshold
    )

  # ----- Create 'data' --------------------------------------------------------

  # data <-
  #   airnow_createData(
  #     locationTbl,
  #     airnow_data
  #   )

  # ----- Return 'monitor' object ----------------------------------------------



}

# ===== DEBUGGING ==============================================================

if ( FALSE ) {


  ptm <- proc.time()

  library(MazamaCoreUtils)
  logger.setLevel(TRACE)

  library(MazamaSpatialUtils)
  MazamaSpatialUtils::setSpatialDataDir("~/Data/Spatial")
  MazamaSpatialUtils::loadSpatialData("EEZCountries")
  MazamaSpatialUtils::loadSpatialData("OSMTimezones")
  MazamaSpatialUtils::loadSpatialData("NaturalEarthAdm1")
  MazamaSpatialUtils::loadSpatialData("USCensusCounties")

  library(AirMonitorIngest)
  setAPIKey("airnow", Sys.getenv("AIRNOW_API_KEY"))

  MazamaLocationUtils::setLocationDataDir("~/Data/known_locations")

  collectionName <- "airnow_PM2.5_sites"
  distanceThreshold <- 100
  parameterName <- "PM2.5"



  locationTbl <- airnow_update(
    collectionName = collectionName,
    distanceThreshold = distanceThreshold,
    parameterName = parameterName
  )

  total_time <- proc.time() - ptm
  print(total_time)

  # Save it!


}
