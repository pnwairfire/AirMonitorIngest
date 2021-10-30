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

  parameterName <- match.arg(parameterName)

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

  # ----- Simplify airnow_sites ------------------------------------------------

  airnow_sites <-

    # Start with airnow_sites
    airnow_sites %>%

    # Filter for our specified parameter
    dplyr::filter(.data$parameterName == !!parameterName) %>%

    # Remove records with missing lons or lats
    dplyr::filter(
      is.finite(.data$longitude),
      is.finite(.data$latitude),
      .data$longitude != 0,
      .data$latitude != 0
    ) %>%

    # Filter for PM2.5 in North America
    dplyr::filter(.data$parameterName == "PM2.5") %>%
    dplyr::filter(.data$GMTOffsetHours < 0) %>%
    dplyr::filter(.data$latitude > 15.0) %>%

    # Add locationID
    dplyr::mutate(
      locationID = MazamaLocationUtils::location_createID(.data$longitude, .data$latitude)
    ) %>%

    # Remove already known locations
    dplyr::filter(!.data$locationID %in% locationTbl$locationID) %>%

    # Drop the "empty" fields
    dplyr::select(-dplyr::starts_with("empty"))

  # ----- Harmonize variables --------------------------------------------------

  newSites_locationTbl <-

    # Start with airnow_sites
    airnow_sites %>%

    # Rename all existing columns with "airnow_"
    dplyr::rename_all(~ gsub("^", "airnow_", .x)) %>%

    # Rename columns where the data exists
    dplyr::rename(
      AQSID = .data$airnow_AQSID,
      locationID = .data$airnow_locationID,
      locationName = .data$airnow_siteName,
      longitude = .data$airnow_longitude,
      latitude = .data$airnow_latitude,
      elevation = .data$airnow_elevation,
      countryCode = .data$airnow_countryCode,
      stateCode = .data$airnow_stateCode,
      countyName = .data$airnow_countyName
    ) %>%

    # Add "known location" columns derived from AirNow columns where possible
    dplyr::mutate(
      timezone = as.character(NA),
      houseNumber = as.character(NA),
      street = as.character(NA),
      city = as.character(NA),
      zip = as.character(NA)
    ) %>%

    # Remove columns we don't want
    dplyr::select(
      -.data$airnow_FIPSStateCode,
      -.data$airnow_GNISCountyCode
    )

  # ----- Reorganize columns -----------------------------------------------------

  # Get "airnow_" columns
  airnow_columns <-
    names(newSites_locationTbl) %>%
    stringr::str_subset("airnow_.*")

  # NOTE:  Include the "AQSID" column for AirNow data
  newColumns <- c(
    "AQSID",
    MazamaLocationUtils::coreMetadataNames,
    airnow_columns
  )

  # Reorder column names
  newSites_locationTbl <-
    newSites_locationTbl %>%
    dplyr::select(dplyr::all_of(newColumns))

  # ----- Fix/add columns ------------------------------------------------------

  # * countyName casing -----

  newSites_locationTbl$countyName <-
    stringr::str_to_title(newSites_locationTbl$countyName)

  # * Invalidate elevation = 0.0 -----

  mask <- newSites_locationTbl$elevation == 0
  newSites_locationTbl$elevation[mask] <- as.numeric(NA)

  # * Add timezones -----

  newSites_locationTbl$timezone <-
    MazamaSpatialUtils::getTimezone(
      newSites_locationTbl$longitude,
      newSites_locationTbl$latitude,
      # NOTE:  EPA has monitors from US, Canada, Mexico, Puerto Rico, Virgin Islands and Guam
      countryCodes = c("US", "CA", "MX", "PR", "VI", "GU"),
      useBuffering = TRUE
    )

  # * Replace countryCodes -----

  # NOTE:  Puerto Rico seems to be the only mis-assigned country code
  mask <- newSites_locationTbl$timezone == "America/Puerto Rico"
  newSites_locationTbl$countryCode[mask] <- "PR"

  # * Replace stateCodes -----

  # NOTE:  AQS 'State Code' values are a mess because they are naively derived
  # NOTE:  from 9-digit AQS code positions and are incorrect for 12-digit AQS codes.

  newSites_locationTbl$stateCode <-
    MazamaSpatialUtils::getStateCode(
      newSites_locationTbl$longitude,
      newSites_locationTbl$latitude,
      # NOTE:  EPA has monitors from US, Canada, Mexico, Puerto Rico, Virgin Islands and Guam
      countryCodes = c("US", "CA", "MX", "PR", "VI", "GU"),
      useBuffering = TRUE
    )

  # * Replace countyNames -----

  # NOTE:  For foreign countries, AirNow is using level-2 names, rather thane
  # NOTE:  level-3 names.

  mask <- newSites_locationTbl$countryCode != "US"
  newSites_locationTbl$countyName[mask] <- as.character(NA)

  # ----- Return ---------------------------------------------------------------

  locationTbl <- dplyr::bind_rows(locationTbl, newSites_locationTbl)

  return(locationTbl)

}

# ===== DEBUGGING ==============================================================

if ( FALSE ) {


  ptm <- proc.time()

  library(MazamaCoreUtils)
  logger.setLevel(TRACE)

  MazamaLocationUtils::setLocationDataDir("~/Data/known_locations")
  MazamaLocationUtils::mazama_initialize("~/Data/Spatial")

  collectionName <- "NEW_airnow_PM2.5_sites"
  distanceThreshold <- 100
  parameterName <- "PM2.5"



  locationTbl <- airnow_updateKnownLocations(
    collectionName = collectionName,
    distanceThreshold = distanceThreshold,
    parameterName = parameterName
  )

  total_time <- proc.time() - ptm
  print(total_time)

  # Save it!


}
