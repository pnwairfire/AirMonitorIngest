#' @export
#' @importFrom utils read.table
#' @importFrom rlang .data
#'
#' @title Update the AirNow sites "known locations" table
#'
#' @description
#' Create a \code{meta} dataframe with AirNow monitor metadata appropriate
#' for use with the \pkg{MazamaTimeSeries} package.
#'
#' The data model is that monitor metadata are stored in a tibble named \code{meta}.
#' with a \code{deviceDeploymentID} unique identifier that is matched by column
#' names in an associated \code{data} file.
#'
#' @param airnow_data Table of monitor data obtained with \code{epa_api_getData()}.
#' @param sites_locationTbl Table of "known locations" produced with \pkg{MazamaLocationUtils}.
#' @param pollutant a named AirNow pollutant.
#'
#' @return Tibble of device-deployment metadata.
#'

airnow_updateSiteLocations <- function(
  airnow_data = NULL,
  collectionName = NULL
) {

  if ( logger.isInitialized() )
    logger.debug(" ----- airnow_updateSiteLocations() ----- ")

  # ----- Validate Parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(airnow_data)
  MazamaCoreUtils::stopIfNull(collectionName)

  locationDataDir <- MazamaLocationUtils::getLocationDataDir()

  MazamaCoreUtils::stopIfNull(
    locationDataDir,
    "locationDataDir is not set. Set it with MazamaLocationUtils::setLocationDataDir()."
  )

  # ----- Simplify airnow_data -------------------------------------------------

  airnow_data <-
    airnow_data %>%
    # Saw some AQSID with two records per hour, one with and one without parameterAQI
    dplyr::arrange(parameterAQI) %>%
    dplyr::distinct(.data$AQSID, .keep_all = TRUE) %>%
    # Remove records with missing or zero lon/lat
    dplyr::filter(
      is.finite(.data$longitude),
      is.finite(.data$latitude),
      .data$longitude != 0,
      .data$latitude != 0
    )

  # ----- Update "known locations" ---------------------------------------------

  sites_locationTbl <- MazamaLocationUtils::table_load(collectionName)

  foundLocationIDs <-
    MazamaLocationUtils::table_getLocationID(
      sites_locationTbl,
      airnow_data$longitude,
      airnow_data$latitude,
      distanceThreshold = 500               # TODO:  Hardcoded value!
    )

  if ( any(is.na(locationIDs)) ) {

    # * Filter for new locations -----
    airnow_data_new <-
      airnow_data %>%
      dplyr::filter(is.na(!!foundLocationIDs))

    # * Load spatial data -----
    MazamaLocationUtils::mazama_initialize()

    # * Add new locations -----
    sites_locationTbl <-
      MazamaLocationUtils::table_addLocation(
        sites_locationTbl,
        airnow_data_new$longitude,
        airnow_data_new$latitude,
        distanceThreshold = 500,            # TODO:  Hardcoded value!
        verbose = FALSE
      )

    # * Add new metadata -----
    locationID <-
      MazamaLocationUtils::table_getLocationID(
        sites_locationTbl,
        airnow_data_new$longitude,
        airnow_data_new$latitude,
        distanceThreshold = 500             # TODO:  Hardcoded value!
      )

    sites_locationTbl <-
      sites_locationTbl %>%
      table_updateColumn(
        "locationName",
        locationID,
        airnow_data_new$siteName
      ) %>%
      table_updateColumn(
        "AQSID",
        locationID,
        airnow_data_new$AQSID
      ) %>%
      table_updateColumn(
        "airnow_agencyName",
        locationID,
        airnow_data_new$agencyName
      )

    # * Save updated table -----
    MazamaLocationUtils::table_save(
      sites_locationTbl,
      collectionName = collectionName,
      backup = TRUE,
      outputType = "rda"
    )

  } # END of new locations

}

# ===== DEBUGGING ==============================================================

if ( FALSE ) {


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

  airnow_data <-
    airnow_api_getData(
      starttime = starttime,
      endtime = endtime,
      timezone = timezone,
      pollutant = pollutant,
      monitorType = monitorType
    )

  airnow_updateSiteLocations(
    airnow_data = airnow_data,
    collectionName = collectionName
  )


}
