#' @export
#' @importFrom utils read.table
#' @importFrom rlang .data
#'
#' @title Update the WRCC known locations table
#'
#' @param wrcc_locationTbl WRCC table of "known locations".
#' @param airnow_locationTbl AirNow table of "known locations".
#' @param distanceThreshold Separation distance in meters between "known locations".
#' @param wrcc_data Tibble of parsed/QC'ed/clustered WRCC data.
#'
#' @description
#' Update a "known locations" table with WRCC monitor metadata.
#'
#' Clustered locations in incoming WRCC data are used to find "known locations"
#' in existing WRCC and AirNow location tables in that order. If no known locations
#' are found, new ones are created.
#'
#' The returned tibble will have all records in the incoming \code{wrcc_locationTbl}
#' as well as any additional records needed for new locations.
#'
#' The returned known locations tibble is then available for use during the c
#' reation of 'meta' dataframes used in \emph{monitor} objects in the
#' \pkg{MazamaTimeSeries} package.
#'
#' @return Tibble of unique "known locations".
#'

wrcc_updateKnownLocations <- function(
  wrcc_locationTbl = NULL,
  airnow_locationTbl = NULL,
  distanceThreshold = NULL,
  wrcc_data = NULL
) {

  logger.debug(" ----- wrcc_updateKnownLocations() ----- ")

  # ----- Validate Parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(wrcc_locationTbl)
  MazamaCoreUtils::stopIfNull(airnow_locationTbl)
  MazamaCoreUtils::stopIfNull(distanceThreshold)
  MazamaCoreUtils::stopIfNull(wrcc_data)

  # ----- Find WRCC locations --------------------------------------------------

  logger.trace("Looking for WRCC locations")

  # Find individual locations (after clustering)
  wrcc_data_locations <-
    wrcc_data %>%
    dplyr::distinct(.data$clusterID, .keep_all = TRUE) %>%
    dplyr::rename(
      longitude = .data$clusterLon,
      latitude = .data$clusterLat
    )

  if ( nrow(wrcc_data_locations) == 0 )
    stop("no locations found in 'wrcc_data'")

  # Add locationID
  wrcc_data_locations$locationID <-
    MazamaLocationUtils::table_getLocationID(
      locationTbl = wrcc_locationTbl,
      longitude = wrcc_data_locations$longitude,
      latitude = wrcc_data_locations$latitude,
      distanceThreshold = distanceThreshold,
      measure = "geodesic"
    )

  # Split known/unknown
  wrcc_known <-
    wrcc_data_locations %>%
    dplyr::filter(!is.na(.data$locationID))

  wrcc_unknown <-
    wrcc_data_locations %>%
    dplyr::filter(is.na(.data$locationID))

  # ----- Find AirNow locations ------------------------------------------------

  if ( nrow(wrcc_unknown) > 0 ) {

    logger.trace("Looking for AirNow locations")

    # Add locationID
    wrcc_data_locations$locationID <-
      MazamaLocationUtils::table_getLocationID(
        locationTbl = airnow_locationTbl,
        longitude = wrcc_data_locations$longitude,
        latitude = wrcc_data_locations$latitude,
        distanceThreshold = distanceThreshold,
        measure = "geodesic"
      )

    # Split known/unknown
    wrcc_airnow_known <-
      wrcc_data_locations %>%
      dplyr::filter(!is.na(.data$locationID))

    wrcc_unknown <-
      wrcc_data_locations %>%
      dplyr::filter(is.na(.data$locationID))

    if ( nrow(wrcc_airnow_known) > 0 ) {

      # Add records for the AirNow known locations
      from_airnowTbl <-
        airnow_locationTbl %>%
        dplyr::filter(.data$locationID %in% wrcc_airnow_known$locationID)

      # Bind new records onto the WRCC known locations table
      wrcc_locationTbl <-
        dplyr::bind_rows(wrcc_locationTbl, from_airnowTbl)

    }

  }

  # ----- Create new "known_locations" records ---------------------------------

  if ( nrow(wrcc_unknown) > 0 ) {

    logger.trace("Creating %d new locations", nrow(wrcc_unknown))

    new_locationTbl <-
      MazamaLocationUtils::table_initializeExisting(
        wrcc_unknown,
        countryCodes = c("CA", "US", "MX", "PR", "VI", "GU"),
        distanceThreshold = distanceThreshold,
        measure = "geodesic",
        verbose = FALSE
      )

    # dplyr::glimpse(new_locationTbl)
    # Rows: 2
    # Columns: 34
    # $ locationID     <chr> "55a69cdf6e91b726", "83712eb6c8f31ceb"
    # $ locationName   <chr> "us.or_55a69c", "us.wa_83712e"
    # $ longitude      <dbl> -122.4866, -119.4341
    # $ latitude       <dbl> 42.74392, 48.71048
    # $ elevation      <dbl> NA, NA
    # $ countryCode    <chr> "US", "US"
    # $ stateCode      <chr> "OR", "WA"
    # $ countyName     <chr> NA, NA
    # $ timezone       <chr> "America/Los_Angeles", "America/Los_Angeles"
    # $ houseNumber    <chr> NA, NA
    # $ street         <chr> NA, NA
    # $ city           <chr> NA, NA
    # $ zip            <chr> NA, NA
    # $ DateTime       <chr> "1508120000", "1509011100"
    # $ GPSLat         <dbl> 42.74385, 48.71049
    # $ GPSLon         <dbl> -122.4865, -119.4341
    # $ Type           <dbl> 9, 9
    # $ SerialNumber   <chr> "-9999", "-9999"
    # $ ConcRT         <dbl> 26, 0
    # $ Conc_l_m       <dbl> NA, NA
    # $ AvAirFlw       <dbl> 2, 2
    # $ AvAirTemp      <dbl> 30.9, 12.1
    # $ RelHumidity    <dbl> 34, 85
    # $ BaromPress     <dbl> 91714, 97106
    # $ SensorIntAT    <dbl> NA, NA
    # $ SensorIntRH    <dbl> 24, 50
    # $ WindSpeed      <dbl> 3.0, 0.6
    # $ WindDir        <dbl> 220, 48
    # $ BatteryVoltage <dbl> 14.0, 14.3
    # $ Alarm          <dbl> 0, 0
    # $ monitorName    <chr> "Smoke #16", "Smoke #16"
    # $ monitorType    <chr> "ESAM", "ESAM"
    # $ datetime       <dttm> 2015-08-12 00:00:00, 2015-09-01 11:00:00
    # $ clusterID      <int> 1, 2

    unwantedColumns <- setdiff(names(new_locationTbl), names(wrcc_locationTbl))

    new_locationTbl <-

      new_locationTbl %>%

      # Add spatial metadata
      dplyr::mutate(
        locationName = .data$monitorName
      ) %>%


      # Remove unwanted columns
      dplyr::select(-dplyr::all_of(unwantedColumns))

    # Bind new records onto the WRCC known locations table
    wrcc_locationTbl <-
      dplyr::bind_rows(wrcc_locationTbl, new_locationTbl)

  }

  # ----- Return ---------------------------------------------------------------

  return(wrcc_locationTbl)

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
  wrcc_locationTbl <- table_initialize()
  airnow_locationTbl <- table_load("airnow_PM2.5_sites")

  library(AirMonitorIngest)

  distanceThreshold <- 1000

  wrcc_data <-
    wrcc_downloadData(
      20150701,
      20150930,
      unitID = "SM16"
    ) %>%
    wrcc_parseData() %>%
    wrcc_qualityControl() %>%
    addClustering(
      clusterDiameter = distanceThreshold,
      lonVar = "GPSLon",
      latVar = "GPSLat",
      maxClusters = 50,
      flagAndKeep = FALSE
    )


  new_tbl <- wrcc_updateKnownLocations(
    wrcc_locationTbl = wrcc_locationTbl,
    airnow_locationTbl = airnow_locationTbl,
    distanceThreshold = distanceThreshold,
    wrcc_data = wrcc_data
  )


}
