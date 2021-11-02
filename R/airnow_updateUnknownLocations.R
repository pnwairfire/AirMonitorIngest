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
#' @param airnow_data Table of monitor data obtained with \code{epa_api_getData()}.
#' @param distanceThreshold Separation distance in meters between "known locations".
#'
#' @return Tibble of device-deployment metadata.
#'

airnow_updateUnknownLocations <- function(
  locationTbl = NULL,
  airnow_data = NULL,
  distanceThreshold = NULL
) {

  if ( logger.isInitialized() )
    logger.debug(" ----- airnow_updateUnknownLocations() ----- ")

  # ----- Validate Parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(locationTbl)
  MazamaCoreUtils::stopIfNull(airnow_data)
  MazamaCoreUtils::stopIfNull(distanceThreshold)

  # ----- Identify new locations -----------------------------------------------

  # NOTE:  Do this to be sure we don't overwrite existing known locations.

  # Add locationID
  airnow_data$locationID <-
    MazamaLocationUtils::table_getLocationID(
      locationTbl = locationTbl,
      longitude = airnow_data$longitude,
      latitude = airnow_data$latitude,
      distanceThreshold = distanceThreshold,
      measure = "geodesic"
    )

  airnow_unknown <-
    airnow_data %>%
    dplyr::filter(is.na(.data$locationID))














  # # ----- Simplify airnow_data -------------------------------------------------
  #
  # airnow_data <-
  #
  #   airnow_data %>%
  #
  #   # Saw some AQSID with two records per hour, one with and one without paramterAQI
  #   dplyr::arrange(parameterAQI) %>%
  #   dplyr::distinct(.data$AQSID, .keep_all = TRUE) %>%
  #
  #   # Remove records with missing or zero lon/lat
  #   dplyr::filter(
  #     is.finite(.data$longitude),
  #     is.finite(.data$latitude),
  #     .data$longitude != 0,
  #     .data$latitude != 0
  #   )
  #
  # # ----- Find nearest known locations -----------------------------------------
  #
  # known_locations <-
  #   MazamaLocationUtils::table_getNearestLocation(
  #     locationTbl,
  #     airnow_data$longitude,
  #     airnow_data$latitude,
  #     500
  #   )
  #
  # # NOTE:  Any airnow_data records that do not match will have missing values
  # # NOTE:  for locationID
  #
  # # ----- Meta for existing locations ------------------------------------------
  #
  # meta_shared <-
  #
  #   known_locations %>%
  #
  #   # All locations found in locationTbl
  #   dplyr::filter(!is.na(.data$locationID)) %>%
  #
  #   # Unique instrument ID = AQSID as we have nothing more specific
  #   dplyr::mutate(
  #     deviceID = .data$AQSID
  #   ) %>%
  #
  #   # Unique "device deployment" ID
  #   dplyr::mutate(
  #     deviceDeploymentID = paste(.data$locationID, .data$deviceID, sep = "_")
  #   ) %>%
  #
  #   # Other required metadata
  #   dplyr::mutate(
  #     deviceType = as.character(NA),
  #     deviceDescription = as.character(NA),
  #     deviceExtra = as.character(NA),
  #     parameterName = !!parameterName,
  #     units = !!units,
  #     dataIngestSource = "AirNow",
  #     dataIngestURL = "https://www.airnowapi.org/aq/data/",
  #     dataIngestUnitID = as.character(NA),
  #     dataIngestExtra = as.character(NA),
  #     dataIngestDescription = as.character(NA)
  #   )


  # ============================================================================
  # ============================================================================
  # ============================================================================
  # TODO:  This function should fail if there are new metadata
  # TODO:  A calling function should call airnow_updateSiteLocations() first and
  # TODO:  then pass the same airnow_data and the updated siteLocationTbl to
  # TODO:  this function.
  # ============================================================================
  # ============================================================================
  # ============================================================================






  # # ----- Meta for new sites ---------------------------------------------------
  #
  # AQSID_new <- setdiff(airnow_data$AQSID, sites_locationTbl$AQSID)
  #
  # airnow_data_new <-
  #   airnow_data %>%
  #   dplyr::filter(.data$AQSID %in% !!AQSID_new)
  #
  # # > print(names(airnow_data_new), width = 75)
  # # [1] "latitude"                  "longitude"
  # # [3] "utcTime"                   "parameterName"
  # # [5] "parameterConcentration"    "parameterUnits"
  # # [7] "parameterRawConcentration" "parameterAQI"
  # # [9] "parameterAQC"              "siteName"
  # # [11] "agencyName"                "AQSID"
  # # [13] "fullAQSID"
  #
  # result <- try({
  #
  #   # * update "known locations" -----
  #
  #   MazamaLocationUtils::mazama_initialize()
  #   sites_locationTbl <-
  #     sites_locationTbl %>%
  #     table_addLocation(
  #       airnow_data_new$longitude,
  #       airnow_data_new$latitude,
  #       distance_threshold = 100,            # TODO:  Hardcoded value!
  #       verbose = FALSE
  #     )
  #
  #   # * save "known locations" -----
  #
  #   MazamaLocationUtils::table_save(
  #     sites_locationTbl,
  #     collectionName = "airnow",
  #     backup = TRUE,
  #     outputType = "rda"
  #   )
  #
  #   # * create 'meta' -----
  #
  #   meta_new <-
  #     sites_locationTbl %>%
  #     dplyr::filter(.data$AQSID %in% !!AQSID_shared) %>%
  #     # Unique instrument ID = AQSID as we have nothing more specific
  #     dplyr::mutate(
  #       deviceID = .data$AQSID
  #     ) %>%
  #     # Unique "device deployment" ID
  #     dplyr::mutate(
  #       deviceDeploymentID = paste(.data$locationID, .data$deviceID, sep = "_")
  #     ) %>%
  #     # Other required metadata
  #     dplyr::mutate(
  #       deviceType = as.character(NA),
  #       deviceDescription = as.character(NA),
  #       deviceExtra = as.character(NA),
  #       parameterName = !!parameterName,
  #       units = !!units,
  #       dataIngestSource = "AirNow",
  #       dataIngestURL = "https://www.airnowapi.org/aq/data/",
  #       dataIngestUnitID = as.character(NA),
  #       dataIngestExtra = as.character(NA),
  #       dataIngestDescription = as.character(NA)
  #     )
  #
  # }, silent = TRUE)
  #
  # if ( "try-error" %in% class(result) ) {
  #
  #   if ( logger.isInitialized() ) {
  #     err_msg <- geterrmessage()
  #     logger.warning(" ----- epa_aqs_createMeta() ----- ")
  #   }
  #
  # }
  #
  # # * combine tibbles -----
  #
  # if ( exists("meta_shared") ) {
  #   meta <- dplyr::bind_rows(meta_shared, meta_new)
  # } else {
  #   meta <- meta_shared
  # }

  # # ----- Reorder columns ------------------------------------------------------
  #
  # coreNames <- AirMonitor::coreMetadataNames
  # missingCoreNames <- setdiff(coreNames, names(meta))
  # airnowNames <- setdiff(names(meta), coreNames)
  #
  # for ( name in missingCoreNames ) {
  #   meta[[name]] <- as.character(NA)
  # }
  #
  # meta <-
  #   meta %>%
  #   dplyr::select(dplyr::all_of(c(coreNames, airnowNames)))
  #
  # # ----- Return ---------------------------------------------------------------
  #
  # return(meta)

}

# ===== DEBUGGING ==============================================================

if ( FALSE ) {




}
