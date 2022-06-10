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
#' The data model has monitor metadata stored in a tibble named \code{meta}.
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

    # Saw some fullAQSIDs with two records per hour, one with and one without parameterAQI
    dplyr::arrange(.data$parameterAQI) %>%
    dplyr::distinct(.data$fullAQSID, .keep_all = TRUE) %>%

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

  # > dplyr::glimpse(airnow_data_locations, width = 75)
  # Rows: 1,214
  # Columns: 30
  # $ locationID            <chr> "a0ae2c387b6e3ec4", "1c8ace316889064c", "b8
  # $ locationName          <chr> "NCore", "A Street", "Kitimat Haul Road", "
  # $ longitude             <dbl> -147.7273, -147.6933, -128.7027, -128.6714,
  # $ latitude              <dbl> 64.84580, 64.84593, 54.02919, 54.05389, 53.
  # $ elevation             <dbl> 132.1, NA, 2.1, NA, NA, 83.0, 600.2, NA, 9.
  # $ countryCode           <chr> "US", "US", "CA", "CA", "CA", "US", "US", "
  # $ stateCode             <chr> "AK", "AK", "BC", "BC", "BC", "OR", "CA", "
  # $ countyName            <chr> "Fairbanks North Star", "Fairbanks North St
  # $ timezone              <chr> "America/Anchorage", "America/Anchorage", "
  # $ houseNumber           <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
  # $ street                <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
  # $ city                  <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
  # $ zip                   <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
  # $ AQSID                 <chr> "020900034", "020900040", "000103902", "000
  # $ fullAQSID             <chr> "840020900034", "840020900040", "1240001039
  # $ airnow_stationID      <chr> "020900034", "840020900040", "000103902", "
  # $ airnow_parameterName  <chr> "PM2.5", "PM2.5", "PM2.5", "PM2.5", "PM2.5"
  # $ airnow_monitorType    <chr> "Permanent", "Permanent", "Permanent", "Per
  # $ airnow_siteCode       <chr> "0034", "0040", "3902", "3901", "3905", "10
  # $ airnow_status         <chr> "Active", "Active", "Active", "Active", "Ac
  # $ airnow_agencyID       <chr> "AK1", "AK1", "BC1", "BC1", "BC1", "TRX", "
  # $ airnow_agencyName     <chr> "State of Alaska DEC", "State of Alaska DEC
  # $ airnow_EPARegion      <chr> "R10", "R10", "CA", "CA", "CA", "R8", "R9",
  # $ airnow_GMTOffsetHours <dbl> -9, -9, -8, -8, -8, -8, -8, -8, -8, -8, -8,
  # $ airnow_CBSA_ID        <chr> "21820", "21820", NA, NA, NA, NA, NA, NA, "
  # $ airnow_CBSA_Name      <chr> " Fairbanks, AK ", " Fairbanks, AK ", NA, N
  # $ airnow_stateAQSCode   <chr> "02", "02", "00", "00", "00", "41", "06", "
  # $ airnow_countyAQSCode  <chr> "090", "090", "010", "010", "010", "011", "
  # $ airnow_FIPSMSACode    <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
  # $ airnow_MSAName        <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,

  # NOTE:  Assume that all the work has been done to update the incoming
  # NOTE:  locationTbl so that all locations in airnow_data are "known".
  # NOTE:
  # NOTE:  Any that are not will be removed with a warning message.

  if ( anyNA(airnow_data_locations$locationID) ) {

    err_msg <- sprintf(
      "%d locations are still unknown and will be removed",
      sum(is.na(airnow_data_locations$locationID))
    )
    if ( logger.isInitialized() ) logger.warn(err_msg)
    warning(err_msg)

    # Retain only truly "known" locations
    mask <- !is.na(airnow_data_locations$locationID)
    airnow_data_locations <- airnow_data_locations[mask,]

    # NOTE:  At this point, there may be duplicate locationIDs associated with
    # NOTE:  different fullAQSIDs in airnow_data. Make sure we are using the latest
    # NOTE:  fullAQSIDs from airnow_data.

    # Add AQSID and fullAQSID
    airnow_data_locations$AQSID <- airnow_data$AQSID[mask]
    airnow_data_locations$fullAQSID <- airnow_data$fullAQSID[mask]

  }

  # ----- Create 'meta' --------------------------------------------------------

  meta <-

    airnow_data_locations %>%

    # Unique device ID = fullAQSID as we have nothing more specific
    dplyr::mutate(
      deviceID = .data$fullAQSID
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
      deploymentType = .data$airnow_monitorType,
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




  meta <-
    airnow_createMeta(
      locationTbl = locationTbl,
      distanceThreshold = distanceThreshold,
      airnow_data = airnow_data
    )


}
