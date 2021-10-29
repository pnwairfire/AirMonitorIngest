#' @export
#' @importFrom utils read.table
#' @importFrom rlang .data
#'
#' @title Update the AirNow "known locations" table
#'
#' @param collectionName Character identifier for this table.
#' @param distanceThreshold Separation distance in meters between "known locations".
#' @param parameterName An EPA AQS criteria parameter name.
#'
#' @description
#' Create/update a "known locations" table with AirNow monitor metadata.
#'
#' The latest "sites metadata" file is downloaded from AirNow and cleaned up so
#' that records can be added to the "known locations" table identified by
#' \code{collectionName}.
#'
#' This file is then available for use during the creation of 'meta' dataframes
#' used in \emph{monitor} objects in the \pkg{MazamaTimeSeries} package.
#'
#' @return Tibble of unique "known locations".
#'

airnow_updateKnownLocations <- function(
  collectionName = NULL,
  distanceThreshold = 100,
  parameterName = c("PM2.5") ###, "CO", "OZONE", "PM10"),
) {

  if ( logger.isInitialized() )
    logger.debug(" ----- airnow_updateKnownLocations() ----- ")

  # ----- Validate Parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(collectionName)

  locationDataDir <- MazamaLocationUtils::getLocationDataDir()

  MazamaCoreUtils::stopIfNull(
    locationDataDir,
    "locationDataDir is not set. Set it with MazamaLocationUtils::setLocationDataDir()."
  )

  locationsFilePath <- file.path(locationDataDir, paste0(collectionName, ".rda"))

  # ----- Load data ------------------------------------------------------------

  if ( file.exists(locationsFilePath) ) {
    locationTbl <- MazamaLocationUtils::table_load(collectionName)
  } else {
    locationTbl <- MazamaLocationUtils::table_initialize()
  }

  result <- try({
    airnow_sites <- airnow_getSites()
  }, silent = TRUE)

  # * error return -----

  if ( "try-error" %in% class(result) ) {
    if ( logger.isInitialized() )
      logger.warn("Failed to get sites metadata from AirNow.")

    return(locationTbl)
  }

  # ----- Find new locations ---------------------------------------------------

  # * Add unique identifiers -----

  airnow_sites$locationID <-
    MazamaLocationUtils::location_createID(
      sites_locationTbl$longitude,
      sites_locationTbl$latitude
    )

  # ----- Harmonize variables --------------------------------------------------

  sites_locationTbl <-

    # Start with airnow_sites
    airnow_sites %>%

    # Drop the "empty" fields
    dplyr::select(-dplyr::starts_with("empty")) %>%

    # Filter for PM2.5 in North America
    dplyr::filter(.data$parameterName == "PM2.5") %>%
    dplyr::filter(.data$GMTOffsetHours < 0) %>%
    dplyr::filter(.data$latitude > 15.0) %>%

    # Rename all existing columns with "airnow_"
    dplyr::rename_all(~ gsub("^", "airnow_", .x)) %>%

    # Rename columns where the data exists
    dplyr::rename(
      locationName = .data$siteName,
      longitude = .data$longitude,
      latitude = .data$latitude,
      elevation = .data$elevation,
      countryCode = .data$countryCode,
      stateCode = .data$stateCode,
      countyName = .data$countyName
    ) %>%

    # Add "known location" columns derived from AirNow columns where possible
    dplyr::mutate(
      locationID = as.character(NA),
      timezone = as.character(NA),
      houseNumber = as.character(NA),
      street = as.character(NA),
      city = as.character(NA),
      zip = as.character(NA)
    ) %>%

    # Remove records with missing lons or lats
    dplyr::filter(
      is.finite(.data$longitude),
      is.finite(.data$latitude),
      .data$longitude != 0,
      .data$latitude != 0
    )

  # ----- Reorganize columns -----------------------------------------------------

  # Get "airnow_" columns
  airnow_columns <-
    names(sites_locationTbl) %>%
    stringr::str_subset("airnow_.*")

  newColumns <- c(
    MazamaLocationUtils::coreMetadataNames,
    airnow_columns
  )

  # Reorder column names
  sites_locationTbl <-
    sites_locationTbl %>%
    dplyr::select(dplyr::all_of(newColumns))

  # ----- Add/fix columns ------------------------------------------------------

  # * Add unique identifiers -----

  sites_locationTbl$locationID <-
    MazamaLocationUtils::location_createID(
      sites_locationTbl$longitude,
      sites_locationTbl$latitude
    )

  # * Add timezones -----

  sites_locationTbl$timezone <-
    MazamaSpatialUtils::getTimezone(
      sites_locationTbl$longitude,
      sites_locationTbl$latitude,
      # NOTE:  EPA has monitors from US, Canada, Mexico, Puerto Rico, Virgin Islands and Guam
      countryCodes = c("US", "CA", "MX", "PR", "VI", "GU"),
      useBuffering = TRUE
    )

  # * Replace countryCodes -----

  # NOTE:  Puerto Rico seems to be the only mis-assigned country code

  sites_locationTbl$countryCode[sites_locationTbl$timezone == "America/Puerto Rico"] <- "PR"

  # * Replace stateCodes -----

  # NOTE:  AQS 'State Code' values are a mess because they are naively derived
  # NOTE:  from 9-digit AQS code positions and are incorrect for 12-digit AQS codes.

  MazamaSpatialUtils::loadSpatialData("NaturalEarthAdm1")

  sites_locationTbl$stateCode <-
    MazamaSpatialUtils::getStateCode(
      sites_locationTbl$longitude,
      sites_locationTbl$latitude,
      # NOTE:  EPA has monitors from US, Canada, Mexico, Puerto Rico, Virgin Islands and Guam
      countryCodes = c("US", "CA", "MX", "PR", "VI", "GU"),
      useBuffering = TRUE
    )






















#
#   foundLocationIDs <-
#     MazamaLocationUtils::table_getLocationID(
#       sites_locationTbl,
#       airnow_data$longitude,
#       airnow_data$latitude,
#       distanceThreshold = distanceThreshold
#     )
#
#   if ( any(is.na(foundLocationIDs)) ) {
#
#     if ( logger.isInitialized() )
#       logger.trace("adding records for %d new locations", length(foundLocationIDs))
#
#     # * Filter for new locations -----
#     airnow_data_new <-
#       airnow_data %>%
#       dplyr::filter(is.na(!!foundLocationIDs))
#
#     # * Load spatial data -----
#     MazamaLocationUtils::mazama_initialize()
#
#     # * Add new locations -----
#     sites_locationTbl <-
#       MazamaLocationUtils::table_addLocation(
#         sites_locationTbl,
#         airnow_data_new$longitude,
#         airnow_data_new$latitude,
#         distanceThreshold = distanceThreshold,
#         verbose = FALSE
#       )
#
#     # * Add new metadata -----
#     locationID <-
#       MazamaLocationUtils::table_getLocationID(
#         sites_locationTbl,
#         airnow_data_new$longitude,
#         airnow_data_new$latitude,
#         distanceThreshold = distanceThreshold
#       )
#
#     sites_locationTbl <-
#       sites_locationTbl %>%
#       table_updateColumn(
#         "locationName",
#         locationID,
#         airnow_data_new$siteName
#       ) %>%
#       table_updateColumn(
#         "AQSID",
#         locationID,
#         airnow_data_new$AQSID
#       ) %>%
#       table_updateColumn(
#         "airnow_agencyName",
#         locationID,
#         airnow_data_new$agencyName
#       )
#
#     # * Save updated table -----
#     MazamaLocationUtils::table_save(
#       sites_locationTbl,
#       collectionName = collectionName,
#       backup = TRUE,
#       outputType = "rda"
#     )
#
#   } # END of new locations

  return(locationTbl)

}

# ===== DEBUGGING ==============================================================

if ( FALSE ) {


  collectionName <- "NEW_airnow_PM2.5_sites"
  distanceThreshold <- 100












  library(MazamaCoreUtils)
  logger.setLevel(TRACE)

  MazamaLocationUtils::setLocationDataDir("~/Data/known_locations")
  collectionName = "airnow_PM2.5_sites"

  library(AirMonitorIngest)
  setAPIKey("airnow", Sys.getenv("AIRNOW_API_KEY"))

  starttime <- 2021102700
  endtime <- 2021102700
  timezone <- "America/Los_Angeles"
  pollutant <- "PM2.5"
  monitorType <- "both"
  distanceThreshold = 500

  airnow_data <-
    airnow_api_getData(
      starttime = starttime,
      endtime = endtime,
      timezone = timezone,
      pollutant = pollutant,
      monitorType = monitorType
    )

  airnow_updateKnownLocations(
    airnow_data = airnow_data,
    collectionName = collectionName,
    distanceThreshold = distanceThreshold
  )


}
