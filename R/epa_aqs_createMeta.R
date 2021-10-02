#' @export
#' @import MazamaCoreUtils
#' @importFrom rlang .data
#'
#' @title Create a 'meta' dataframe with required monitor metadata
#'
#' @description
#' Create a \code{meta} dataframe with EPA AQS monitor metadata appropriate
#' for use with the \pkg{MazamaTimeSeries} package.
#'
#' The data model is that monitor metadata are stored in a tibble named \code{meta}.
#' with a \code{deviceDeploymentID} unique identifier that is matched by column
#' names in an associated \code{data} file.
#'
#' @param AQS_monitors Table of monitor data obtained with \code{epa_aqs_getMonitors()}.
#' @param sites_locationTbl Table of "known locations" produced with \pkg{MazamaLocationUtils}.
#' @param parameterCode EPA "Parameter Code".
#'
#' @return Restructured tibble  with data organized as timeseries columns.
#'

epa_aqs_createMeta <- function(
  AQS_monitors = NULL,
  sites_locationTbl = NULL,
  parameterCode = NULL
) {

  if ( logger.isInitialized() )
    logger.debug(" ----- epa_aqs_createMeta() ----- ")

  # ----- Validate Parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(AQS_monitors)
  MazamaCoreUtils::stopIfNull(sites_locationTbl)
  MazamaCoreUtils::stopIfNull(parameterCode)

  # ----- Prepare data ---------------------------------------------------------

  # * subset for parameterCode -----

  AQS_monitors <-
    AQS_monitors %>%
    # Subset
    dplyr::filter(.data$`Parameter Code` == parameterCode) %>%
    # Add AQSID
    dplyr::mutate(
      AQSID = paste0(.data$`State Code`, .data$`County Code`, .data$`Site Number`)
    ) %>%
    # Add locationID
    dplyr::mutate(
      locationID = MazamaLocationUtils::location_createID(.data$`Longitude`, .data$`Latitude`)
    )

  # * modify columns -----

  monitors_meta <-
    AQS_monitors %>%

    # Rename all existing columns with "AQS_"
    dplyr::rename_all(make.names) %>%
    dplyr::rename_all(~ gsub("^", "AQS_", .x))

  # Keep the following columns
  keep_columns <- c(
    # "AQS_State.Code",
    # "AQS_County.Code",
    # "AQS_Site.Number",
    "AQS_Parameter.Code",
    "AQS_Parameter.Name",
    "AQS_POC",
    # "AQS_Latitude",
    # "AQS_Longitude",
    # "AQS_Datum",
    "AQS_First.Year.of.Data",
    "AQS_Last.Sample.Date",
    "AQS_Monitor.Type",
    "AQS_Networks",
    "AQS_Reporting.Agency",
    "AQS_PQAO",
    "AQS_Collecting.Agency",
    "AQS_Exclusions",
    "AQS_Monitoring.Objective",
    "AQS_Last.Method.Code",
    "AQS_Last.Method",
    "AQS_Measurement.Scale",
    "AQS_Measurement.Scale.Definition",
    "AQS_NAAQS.Primary.Monitor",
    "AQS_QA.Primary.Monitor",
    # "AQS_Local.Site.Name",
    # "AQS_Address",
    # "AQS_State.Name",
    # "AQS_County.Name",
    # "AQS_City.Name",
    # "AQS_CBSA.Name",
    # "AQS_Tribe.Name",
    # "AQS_Extraction.Date",
    "AQS_AQSID",
    "AQS_locationID"
  )

  monitors_meta <-
    monitors_meta %>%
    dplyr::select(dplyr::all_of(keep_columns))


  # ----- Add 'known location' data --------------------------------------------

  # * deviceDeploymentID -----

  monitors_meta <-
    monitors_meta %>%
    # Include 'locationID' so we have something to join on
    dplyr::rename(
      locationID = .data$AQS_locationID,
    ) %>%
    # Unique instrument ID
    dplyr::mutate(
      deviceID = sprintf("%s_%02d", .data$AQS_AQSID, as.numeric(.data$AQS_POC))
    ) %>%
    # Unique "device deployment" ID
    dplyr::mutate(
      deviceDeploymentID = paste(.data$locationID, .data$deviceID, sep = "_")
    ) %>%
    # Now remove AQS_AQSID
    dplyr::select(- .data$AQS_AQSID)

  # * left_join -----

  meta <-
    dplyr::left_join(monitors_meta, sites_locationTbl, by = "locationID")

  # * reorder columns -----

  coreNames <- c(
    "deviceDeploymentID", "deviceID",
    MazamaLocationUtils::coreMetadataNames
  )
  aqsNames <- setdiff(names(meta), coreNames)

  meta <-
    meta %>%
    dplyr::select(dplyr::all_of(c(coreNames, aqsNames)))

  # ----- Return ---------------------------------------------------------------

  return(meta)

}

# ===== DEBUGGING ==============================================================

if ( FALSE ) {

  sites_locationTbl <- MazamaLocationUtils::table_load("AQS_88101_sites")
  AQS_monitors <- epa_aqs_getMonitors(downloadDir = "~/Data/EPA")
  parameterCode <- "88101"

}

