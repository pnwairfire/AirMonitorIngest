#' #' @export
#' #' @importFrom utils read.table
#' #' @importFrom rlang .data
#' #'
#' #' @title Update the AirNow "known locations" table
#' #'
#' #' @description
#' #' Create a \code{meta} dataframe with AirNow monitor metadata appropriate
#' #' for use with the \pkg{MazamaTimeSeries} package.
#' #'
#' #' The data model has monitor metadata stored in a tibble named \code{meta}.
#' #' with a \code{deviceDeploymentID} unique identifier. This identifier is used
#' #' as column names in an associated \code{data} file containing time series
#' #' measurements.
#' #'
#' #' @param airnow_data Table of monitor data obtained with \code{epa_api_getData()}.
#' #' @param collectionName Character identifier for this table.
#' #' @param distanceThreshold Distance in meters.
#' #'
#' #' @return Tibble of device-deployment metadata.
#' #'
#'
#' airnow_updateKnownLocationsTable <- function(
#'   airnow_data = NULL,
#'   collectionName = NULL,
#'   distanceThreshold = 500
#' ) {
#'
#'   if ( logger.isInitialized() )
#'     logger.debug(" ----- airnow_updateKnownLocationsTable() ----- ")
#'
#'   # ----- Validate Parameters --------------------------------------------------
#'
#'   MazamaCoreUtils::stopIfNull(airnow_data)
#'   MazamaCoreUtils::stopIfNull(collectionName)
#'
#'   locationDataDir <- MazamaLocationUtils::getLocationDataDir()
#'
#'   MazamaCoreUtils::stopIfNull(
#'     locationDataDir,
#'     "locationDataDir is not set. Set it with MazamaLocationUtils::setLocationDataDir()."
#'   )
#'
#'   # ----- Simplify airnow_data -------------------------------------------------
#'
#'   airnow_data <-
#'     airnow_data %>%
#'     # Saw some AQSID with two records per hour, one with and one without parameterAQI
#'     dplyr::arrange(parameterAQI) %>%
#'     dplyr::distinct(.data$AQSID, .keep_all = TRUE) %>%
#'     # Remove records with missing or zero lon/lat
#'     dplyr::filter(
#'       is.finite(.data$longitude),
#'       is.finite(.data$latitude),
#'       .data$longitude != 0,
#'       .data$latitude != 0
#'     )
#'
#'   # ----- Update "known locations" ---------------------------------------------
#'
#'   sites_locationTbl <- MazamaLocationUtils::table_load(collectionName)
#'
#'   foundLocationIDs <-
#'     MazamaLocationUtils::table_getLocationID(
#'       sites_locationTbl,
#'       airnow_data$longitude,
#'       airnow_data$latitude,
#'       distanceThreshold = distanceThreshold
#'     )
#'
#'   if ( any(is.na(foundLocationIDs)) ) {
#'
#'     if ( logger.isInitialized() )
#'       logger.trace("adding records for %d new locations", length(foundLocationIDs))
#'
#'     # * Filter for new locations -----
#'     airnow_data_new <-
#'       airnow_data %>%
#'       dplyr::filter(is.na(!!foundLocationIDs))
#'
#'     # * Load spatial data -----
#'     MazamaLocationUtils::mazama_initialize()
#'
#'     # * Add new locations -----
#'     sites_locationTbl <-
#'       MazamaLocationUtils::table_addLocation(
#'         sites_locationTbl,
#'         airnow_data_new$longitude,
#'         airnow_data_new$latitude,
#'         distanceThreshold = distanceThreshold,
#'         verbose = FALSE
#'       )
#'
#'     # * Add new metadata -----
#'     locationID <-
#'       MazamaLocationUtils::table_getLocationID(
#'         sites_locationTbl,
#'         airnow_data_new$longitude,
#'         airnow_data_new$latitude,
#'         distanceThreshold = distanceThreshold
#'       )
#'
#'     sites_locationTbl <-
#'       sites_locationTbl %>%
#'       table_updateColumn(
#'         "locationName",
#'         locationID,
#'         airnow_data_new$siteName
#'       ) %>%
#'       table_updateColumn(
#'         "AQSID",
#'         locationID,
#'         airnow_data_new$AQSID
#'       ) %>%
#'       table_updateColumn(
#'         "airnow_agencyName",
#'         locationID,
#'         airnow_data_new$agencyName
#'       )
#'
#'     # * Save updated table -----
#'     MazamaLocationUtils::table_save(
#'       sites_locationTbl,
#'       collectionName = collectionName,
#'       backup = TRUE,
#'       outputType = "rda"
#'     )
#'
#'   } # END of new locations
#'
#'   return(sites_locationTbl)
#'
#' }
#'
#' # ===== DEBUGGING ==============================================================
#'
#' if ( FALSE ) {
#'
#'
#'   library(MazamaCoreUtils)
#'   logger.setLevel(TRACE)
#'
#'   MazamaLocationUtils::setLocationDataDir("~/Data/known_locations")
#'   collectionName = "airnow_PM2.5_sites"
#'
#'   library(AirMonitorIngest)
#'   setAPIKey("airnow", Sys.getenv("AIRNOW_API_KEY"))
#'
#'   starttime <- 2021102700
#'   endtime <- 2021102700
#'   timezone <- "America/Los_Angeles"
#'   pollutant <- "PM2.5"
#'   monitorType <- "both"
#'   distanceThreshold = 500
#'
#'   airnow_data <-
#'     airnow_api_getData(
#'       starttime = starttime,
#'       endtime = endtime,
#'       timezone = timezone,
#'       pollutant = pollutant,
#'       monitorType = monitorType
#'     )
#'
#'   airnow_updateKnownLocationsTable(
#'     airnow_data = airnow_data,
#'     collectionName = collectionName,
#'     distanceThreshold = distanceThreshold
#'   )
#'
#'
#' }
