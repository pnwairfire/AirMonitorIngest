#' @export
#' @importFrom utils read.table
#' @importFrom rlang .data
#'
#' @title Update the AIRSIS known locations table
#'
#' @param airsis_locationTbl AIRSIS table of "known locations".
#' @param airnow_locationTbl AirNow table of "known locations".
#' @param distanceThreshold Separation distance in meters between "known locations".
#' @param airsis_data Tibble of parsed/QC'ed/clustered AIRSIS data.
#'
#' @description
#' Update a "known locations" table with AIRSIS monitor metadata.
#'
#' Clustered locations in incoming AIRSIS data are used to find "known locations"
#' in existing AIRSIS and AirNow location tables in that order. If no known locations
#' are found, new ones are created.
#'
#' The returned tibble will have all records in the incoming \code{airsis_locationTbl}
#' as well as any additional records needed for new locations.
#'
#' The returned known locations tibble is then available for use during the c
#' reation of 'meta' dataframes used in \emph{monitor} objects in the
#' \pkg{MazamaTimeSeries} package.
#'
#' @return Tibble of unique "known locations".
#'

airsis_updateKnownLocations <- function(
  airsis_locationTbl = NULL,
  airnow_locationTbl = NULL,
  distanceThreshold = NULL,
  airsis_data = NULL
) {

  logger.debug(" ----- airsis_updateKnownLocations() ----- ")

  # ----- Validate Parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(airsis_locationTbl)
  MazamaCoreUtils::stopIfNull(airnow_locationTbl)
  MazamaCoreUtils::stopIfNull(distanceThreshold)
  MazamaCoreUtils::stopIfNull(airsis_data)

  # ----- Find AIRSIS locations --------------------------------------------------

  logger.trace("Looking for AIRSIS locations")

  # Find individual locations (after clustering)
  airsis_data_locations <-
    airsis_data %>%
    dplyr::distinct(.data$clusterID, .keep_all = TRUE)

  if ( nrow(airsis_data_locations) == 0 )
    stop("no locations found in 'airsis_data'")

  # Add locationID
  airsis_data_locations$locationID <-
    MazamaLocationUtils::table_getLocationID(
      locationTbl = airsis_locationTbl,
      longitude = airsis_data_locations$longitude,
      latitude = airsis_data_locations$latitude,
      distanceThreshold = distanceThreshold,
      measure = "geodesic"
    )

  # Split known/unknown
  airsis_known <-
    airsis_data_locations %>%
    dplyr::filter(!is.na(.data$locationID))

  airsis_unknown <-
    airsis_data_locations %>%
    dplyr::filter(is.na(.data$locationID))

  # ----- Find AirNow locations ------------------------------------------------

  if ( nrow(airsis_unknown) > 0 ) {

    logger.trace("Looking for AirNow locations")

    # Add locationID
    airsis_data_locations$locationID <-
      MazamaLocationUtils::table_getLocationID(
        locationTbl = airnow_locationTbl,
        longitude = airsis_data_locations$longitude,
        latitude = airsis_data_locations$latitude,
        distanceThreshold = distanceThreshold,
        measure = "geodesic"
      )

    # Split known/unknown
    airsis_airnow_known <-
      airsis_data_locations %>%
      dplyr::filter(!is.na(.data$locationID))

    airsis_unknown <-
      airsis_data_locations %>%
      dplyr::filter(is.na(.data$locationID))

    if ( nrow(airsis_airnow_known) > 0 ) {

      # Add records for the AirNow known locations
      from_airnowTbl <-
        airnow_locationTbl %>%
        dplyr::filter(.data$locationID %in% airsis_airnow_known$locationID)

      # Bind new records onto the AIRSIS known locations table
      airsis_locationTbl <-
        dplyr::bind_rows(airsis_locationTbl, from_airnowTbl)

    }

  }

  # ----- Create new "known_locations" records ---------------------------------

  if ( nrow(airsis_unknown) > 0 ) {

    logger.trace("Creating %d new locations", nrow(airsis_unknown))

    new_locationTbl <-
      MazamaLocationUtils::table_initializeExisting(
        airsis_unknown,
        countryCodes = c("CA", "US", "MX", "PR", "VI", "GU"),
        distanceThreshold = distanceThreshold,
        measure = "geodesic",
        verbose = FALSE
      )

    # > dplyr::glimpse(new_locationTbl)
    # Rows: 1
    # Columns: 25
    # $ locationID        <chr> "906b5197e3a070e4"
    # $ locationName      <chr> "us.ca_906b51"
    # $ longitude         <dbl> -119.784
    # $ latitude          <dbl> 37.67461
    # $ elevation         <dbl> NA
    # $ countryCode       <chr> "US"
    # $ stateCode         <chr> "CA"
    # $ countyName        <chr> NA
    # $ timezone          <chr> "America/Los_Angeles"
    # $ houseNumber       <chr> NA
    # $ street            <chr> NA
    # $ city              <chr> NA
    # $ zip               <chr> NA
    # $ datetime          <dttm> 2013-05-22 22:00:00
    # $ flow              <dbl> 0.834
    # $ AT                <dbl> 19.5
    # $ RHi               <dbl> 13
    # $ pm25              <dbl> 1
    # $ airsis_Alias      <chr> "NPS YOS1001 Bam"
    # $ airsis_dataFormat <chr> "BAM.1020"
    # $ clusterLon        <dbl> -119.784
    # $ clusterLat        <dbl> 37.67459
    # $ clusterID         <int> 1
    # $ airsis_provider   <chr> "apcd"
    # $ airsis_unitID     <dbl> 1069

    unwantedColumns <- setdiff(names(new_locationTbl), names(airsis_locationTbl))

    new_locationTbl <-

      new_locationTbl %>%

      # Remove unwanted columns
      dplyr::select(-dplyr::all_of(unwantedColumns))

    # Bind new records onto the AIRSIS known locations table
    airsis_locationTbl <-
      dplyr::bind_rows(airsis_locationTbl, new_locationTbl)

  }

  # ----- Return ---------------------------------------------------------------

  return(airsis_locationTbl)

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
  startdate <- MazamaCoreUtils::parseDatetime("2013-05-20", timezone = "UTC")
  enddate <- MazamaCoreUtils::parseDatetime("2013-05-30", timezone = "UTC")
  timezone <- "UTC"
  provider <- "APCD"
  unitID <- "1012"

  airsis_data <-
    airsis_downloadData(
     startdate,
      enddate,
      timezone,
      provider,
      unitID
    ) %>%
    airsis_parseData(
      codeDir = "airsis_codeDir"
    ) %>%
    addClustering(
      clusterDiameter = distanceThreshold,
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
