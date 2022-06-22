#' @export
#' @importFrom utils read.table
#' @importFrom rlang .data
#'
#' @title Update the AirNow "known locations" table
#'
#' @param locationTbl Table of "known locations".
#' @param distanceThreshold Separation distance in meters between "known locations".
#' @param parameterName An EPA AQS criteria parameter name.
#'
#' @description
#' Update a "known locations" table with AirNow monitor metadata.
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
  locationTbl = NULL,
  distanceThreshold = 500,
  parameterName = c("PM2.5") ###, "CO", "OZONE", "PM10")
) {

  if ( logger.isInitialized() )
    logger.debug(" ----- airnow_updateKnownLocations() ----- ")

  # ----- Validate Parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(locationTbl)

  parameterName <- match.arg(parameterName)

  # ----- Load data ------------------------------------------------------------

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

    # Filter for "Active" sites
    dplyr::filter(.data$status == "Active") %>%

    # Remove records with missing longitude or latitude
    dplyr::filter(
      is.finite(.data$longitude),
      is.finite(.data$latitude),
      .data$longitude != 0,
      .data$latitude != 0
    ) %>%

    # Filter for North America
    dplyr::filter(.data$GMTOffsetHours < 0) %>%
    dplyr::filter(.data$latitude > 15.0) %>%
    dplyr::filter(.data$longitude < -50.0) %>%

    # Add locationID
    dplyr::mutate(
      locationID = MazamaLocationUtils::location_createID(.data$longitude, .data$latitude)
    )

  # Remove already known locations
  if ( !is.null(locationTbl) ) {
    airnow_sites <-
      airnow_sites %>%
      dplyr::filter(!.data$locationID %in% locationTbl$locationID)
  }

  # > dplyr::glimpse(airnow_sites, width = 75)
  # Rows: 1,583
  # Columns: 23
  # $ stationID         <chr> "CC0010102", "CC0010301", "CC0010401", "CC00105
  # $ AQSID             <chr> "000010102", "000010301", "000010401", "0000105
  # $ fullAQSID         <chr> "124CC0010102", "124CC0010301", "124CC0010401",
  # $ parameterName     <chr> "PM2.5", "PM2.5", "PM2.5", "PM2.5", "PM2.5", "P
  # $ monitorType       <chr> "Permanent", "Permanent", "Permanent", "Permane
  # $ siteCode          <chr> "0102", "0301", "0401", "0501", "0602", "0901",
  # $ siteName          <chr> "St. John's", "Cornerbrook", "Mount Pearl", "Gr
  # $ status            <chr> "Active", "Active", "Active", "Active", "Active
  # $ agencyID          <chr> "NL1", "NL1", "NL1", "NL1", "NL1", "NL1", "NL1"
  # $ agencyName        <chr> "Newfoundland & Labrador DEC", "Newfoundland &
  # $ EPARegion         <chr> "CA", "CA", "CA", "CA", "CA", "CA", "CA", "CA",
  # $ latitude          <dbl> 47.56038, 48.94940, 47.50513, 48.92696, 48.9522
  # $ longitude         <dbl> -52.71150, -58.05560, -52.79480, -55.65970, -57
  # $ elevation         <dbl> 9.8, NA, NA, NA, NA, 0.9, NA, NA, 19.8, 33.9, 1
  # $ GMTOffsetHours    <dbl> -3.5, -4.0, -3.5, -3.5, -3.5, -3.5, -4.0, -4.0,
  # $ countryFIPS       <chr> "CA", "CA", "CA", "CA", "CA", "CA", "CA", "CA",
  # $ CBSA_ID           <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
  # $ CBSA_Name         <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
  # $ stateAQSCode      <chr> "00", "00", "00", "00", "00", "00", "00", "00",
  # $ stateAbbreviation <chr> "CC", "CC", "CC", "CC", "CC", "CC", "CC", "CC",
  # $ countyAQSCode     <chr> "001", "001", "001", "001", "001", "001", "001"
  # $ countyName        <chr> "NEWFOUNDLAND", "NEWFOUNDLAND", "NEWFOUNDLAND",
  # $ locationID        <chr> "61aa22b8558f3212", "cec69fd52886ae58", "c8ac67

  # ===== NEW SITES FOUND ======================================================

  if ( nrow(airnow_sites) > 0 ) {

    logger.trace("Adding %d new locations", nrow(airnow_sites))

    # ----- Warn of duplicates -------------------------------------------------

    # NOTE:  It is possible to have duplicated locationIDs in the airnow_sites
    # NOTE:  tbl at this point. This may be due to errors in the lat/lon values
    # NOTE:  at this site or multiple active monitors at a single location.
    # NOTE:
    # NOTE:  This breaks the idea that this is "spatial-only" information but we
    # NOTE:  keep these extra records around so we can look up information by
    # NOTE:  fullAQSID.

    duplicate_locationIDs <-
      airnow_sites$locationID[duplicated(airnow_sites$locationID)]

    if ( length(duplicate_locationIDs) > 0 ) {
      for ( locationID in duplicate_locationIDs ) {
        fullAQSIDs <-
          airnow_sites %>%
          dplyr::filter(.data$locationID == !!locationID) %>%
          dplyr::pull(.data$fullAQSID)
        logger.warn("Duplicate active fullAQSIDs at locationID %s: %s", locationID, paste0(fullAQSIDs, collapse = ", "))
      }
    }

    # ----- Harmonize variables ------------------------------------------------

    newSites_locationTbl <-

      # Start with airnow_sites
      airnow_sites %>%

      # Rename all existing columns with "airnow_"
      dplyr::rename_all(~ gsub("^", "airnow_", .x)) %>%

      # Rename columns where the data exists
      dplyr::rename(
        AQSID = .data$airnow_AQSID,
        fullAQSID = .data$airnow_fullAQSID,
        locationID = .data$airnow_locationID,
        locationName = .data$airnow_siteName,
        longitude = .data$airnow_longitude,
        latitude = .data$airnow_latitude,
        elevation = .data$airnow_elevation,
        countryCode = .data$airnow_countryFIPS,     # SEE BELOW:  Convert airnow_countryFIPS to countryCode
        stateCode = .data$airnow_stateAbbreviation, # SEE BELOW:  Convert airnow_stateAbbreviation to stateCode
        countyName = .data$airnow_countyName
      ) %>%

      MazamaLocationUtils::table_addCoreMetadata()

    # ----- Reorganize columns -------------------------------------------------

    # Get "airnow_" columns
    airnow_columns <-
      names(newSites_locationTbl) %>%
      stringr::str_subset("airnow_.*")

    # NOTE:  Include the "AQSID" columns for AirNow data
    newColumns <- c(
      "AQSID",
      "fullAQSID",
      MazamaLocationUtils::coreMetadataNames,
      airnow_columns
    )

    # Reorder column names
    newSites_locationTbl <-
      newSites_locationTbl %>%
      dplyr::select(dplyr::all_of(newColumns))

    # ----- Fix/add columns ----------------------------------------------------

    # * countyName casing -----

    newSites_locationTbl$countyName <-
      stringr::str_to_title(newSites_locationTbl$countyName)

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
    mask <- newSites_locationTbl$timezone == "America/Puerto_Rico"
    newSites_locationTbl$countryCode[mask] <- "PR"

    # * Replace stateCodes -----

    # NOTE:  The use of airnow_stateAbbreviation as the stateCode is only
    # NOTE:  correct when the now corrected countryCode == "US".
    # NOTE:  At least once, "MM" was seen as a US "stateAbbreviation"

    mask <-
      ( newSites_locationTbl$countryCode != "US" ) |
      ( newSites_locationTbl$countryCode != "US" & newSites_locationTbl$stateCode == "MM" ) |
      ( is.na(newSites_locationTbl$stateCode) )

    newSites_locationTbl$stateCode[mask] <-
      MazamaSpatialUtils::getStateCode(
        newSites_locationTbl$longitude[mask],
        newSites_locationTbl$latitude[mask],
        # NOTE:  AirNow has monitors from US, Canada, Mexico, Puerto Rico, Virgin Islands and Guam
        countryCodes = c("US", "CA", "MX", "PR", "VI", "GU"),
        useBuffering = TRUE
      )


    # * Replace countyNames -----

    # NOTE:  For foreign countries, AirNow is using level-2 names, rather than
    # NOTE:  level-3 names.

    mask <- newSites_locationTbl$countryCode != "US"
    newSites_locationTbl$countyName[mask] <- as.character(NA)

    # * Replace locationNames -----

    # NOTE:  "N/A" was seen a few times

    mask <- newSites_locationTbl$locationName == "N/A"
    newSites_locationTbl$locationName[mask] <- as.character(NA)

    # * Combine with locationTbl -----

    locationTbl <- dplyr::bind_rows(locationTbl, newSites_locationTbl)


  }

  # ===== END NEW SITES FOUND ==================================================

  # ----- Return ---------------------------------------------------------------

  return(locationTbl)

}

# ===== DEBUGGING ==============================================================

if ( FALSE ) {


  ptm <- proc.time()

  library(MazamaCoreUtils)
  logger.setLevel(TRACE)

  library(MazamaSpatialUtils)
  MazamaSpatialUtils::setSpatialDataDir("~/Data/Spatial")
  MazamaSpatialUtils::loadSpatialData("EEZCountries.rda")
  MazamaSpatialUtils::loadSpatialData("OSMTimezones.rda")
  MazamaSpatialUtils::loadSpatialData("NaturalEarthAdm1.rda")

  library(AirMonitorIngest)
  setAPIKey("airnow", Sys.getenv("AIRNOW_API_KEY"))

  download.file(
    url = "https://airfire-data-exports.s3-us-west-2.amazonaws.com/monitoring/v2/known-locations/airnow_PM2.5_sites.rda",
    destfile = "~/Data/known_locations/airnow_PM2.5_sites.rda"
  )

  MazamaLocationUtils::setLocationDataDir("~/Data/known_locations")

  collectionName <- "airnow_PM2.5_sites"
  distanceThreshold <- 500
  parameterName <- "PM2.5"

  locationTbl <- MazamaLocationUtils::table_load(collectionName)

  locationTbl <- airnow_updateKnownLocations(
    locationTbl = locationTbl,
    distanceThreshold = distanceThreshold,
    parameterName = parameterName
  )

  total_time <- proc.time() - ptm
  print(total_time)

  # Save it!


}
