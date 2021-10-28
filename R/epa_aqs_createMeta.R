#' @export
#' @import MazamaCoreUtils
#' @importFrom rlang .data
#' @importFrom dplyr all_of
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
#' @return Tibble of device-deployment metadata.
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
      locationID = MazamaCoreUtils::createLocationID(.data$`Longitude`, .data$`Latitude`)
    )

  # * rename AQS columns -----

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
    # Now remove AQS_AQSID which is found in sites_locationTbl
    dplyr::select(- .data$AQS_AQSID)

  # * left_join -----

  meta <-
    dplyr::left_join(monitors_meta, sites_locationTbl, by = "locationID")

  coreNames <- AirMonitor::coreMetadataNames
  missingCoreNames <- setdiff(coreNames, names(meta))
  aqsNames <- setdiff(names(meta), coreNames)

  for ( name in missingCoreNames ) {
    meta[[name]] <- as.character(NA)
  }

  # * reorder columns -----

  meta <-
    meta %>%
    dplyr::select(dplyr::all_of(c(coreNames, aqsNames)))

  # ----- Add missing core metadata --------------------------------------------

  #   > dplyr::glimpse(meta, width = 75)
  #   Rows: 3,523
  #   Columns: 52
  #   $ deviceDeploymentID               <chr> "91d2e5b1abd8dd01_010030010_01",
  #   $ deviceID                         <chr> "010030010_01", "010270001_01",
  #   $ deviceType                       <chr> NA, NA, NA, NA, NA, NA, NA, NA,
  #   $ deviceDescription                <chr> NA, NA, NA, NA, NA, NA, NA, NA,
  #   $ deviceExtra                      <chr> NA, NA, NA, NA, NA, NA, NA, NA,
  #   $ pollutant                        <chr> NA, NA, NA, NA, NA, NA, NA, NA,
  #   $ units                            <chr> NA, NA, NA, NA, NA, NA, NA, NA,
  #   $ locationID                       <chr> "91d2e5b1abd8dd01", "bcec1416572
  #   $ locationName                     <chr> "Fairhope, Alabama", "Ashland",
  #   $ longitude                        <dbl> -87.88026, -85.80361, -87.63810,
  #   $ latitude                         <dbl> 30.49748, 33.28493, 34.76262, 34
  #   $ elevation                        <dbl> 37.19, 344.42, 122.00, 122.00, 5
  #   $ countryCode                      <chr> "US", "US", "US", "US", "US", "U
  #   $ stateCode                        <chr> "AL", "AL", "AL", "AL", "AL", "A
  #   $ countyName                       <chr> "Baldwin", "Clay", "Colbert", "C
  #   $ timezone                         <chr> "America/Chicago", "America/Chic
  #   $ houseNumber                      <chr> NA, NA, NA, NA, NA, NA, NA, NA,
  #   $ street                           <chr> NA, NA, NA, NA, NA, NA, NA, NA,
  #   $ city                             <chr> "Fairhope", "Ashland", "Muscle S
  #   $ zip                              <chr> "36532", "36251", "35661", "3566
  #   $ dataIngestSource                 <chr> NA, NA, NA, NA, NA, NA, NA, NA,
  #   $ dataIngestURL                    <chr> NA, NA, NA, NA, NA, NA, NA, NA,
  #   $ dataIngestUnitID                 <chr> NA, NA, NA, NA, NA, NA, NA, NA,
  #   $ dataIngestExtra                  <chr> NA, NA, NA, NA, NA, NA, NA, NA,
  #   $ dataIngestDescription            <chr> NA, NA, NA, NA, NA, NA, NA, NA,
  #   $ AQS_Parameter.Code               <chr> "88101", "88101", "88101", "8810
  #   $ AQS_Parameter.Name               <chr> "PM2.5 - Local Conditions", "PM2
  #   $ AQS_POC                          <chr> "1", "1", "1", "3", "1", "1", "1
  #   $ AQS_First.Year.of.Data           <dbl> 2000, 1999, 1999, 2009, 1999, 20
  #   $ AQS_Last.Sample.Date             <chr> "2020-12-29", "2020-12-29", "201
  #   $ AQS_Monitor.Type                 <chr> "SLAMS", "SLAMS", "SLAMS", "SLAM
  #   $ AQS_Networks                     <chr> NA, NA, NA, NA, NA, NA, NA, NA,
  #   $ AQS_Reporting.Agency             <chr> "Al Dept Of Env Mgt", "Al Dept O
  #   $ AQS_PQAO                         <chr> "Al Dept Of Env Mgt", "Al Dept O
  #   $ AQS_Collecting.Agency            <chr> "Al Dept Of Env Mgt", "Al Dept O
  #   $ AQS_Exclusions                   <chr> NA, NA, NA, "All (2009-04-03 - 2
  #   $ AQS_Monitoring.Objective         <chr> "POPULATION EXPOSURE", "REGIONAL
  #   $ AQS_Last.Method.Code             <chr> "145", "145", "145", "170", "145
  #   $ AQS_Last.Method                  <chr> "R & P Model 2025 PM-2.5 Sequent
  #   $ AQS_Measurement.Scale            <chr> "NEIGHBORHOOD", "REGIONAL SCALE"
  #   $ AQS_Measurement.Scale.Definition <chr> "500 M TO 4KM", "50 TO HUNDREDS
  #   $ AQS_NAAQS.Primary.Monitor        <chr> "Y", "Y", NA, NA, "Y", NA, "Y",
  #   $ AQS_QA.Primary.Monitor           <chr> NA, NA, NA, NA, NA, NA, "Y", NA,
  #   $ AQS_Site.Established.Date        <chr> "2000-01-01", "1991-03-25", "197
  #   $ AQS_Site.Closed.Date             <chr> NA, NA, "2019-11-01", "2019-11-0
  #   $ AQS_GMT.Offset                   <dbl> -6, -6, -6, -6, -6, -6, -6, -6,
  #   $ AQS_Owning.Agency                <chr> "Al Dept Of Env Mgt", "Al Dept O
  #   $ AQS_Address                      <chr> "FAIRHOPE HIGH SCHOOL, 1 PIRATE
  #   $ AQS_CBSA.Name                    <chr> "Daphne-Fairhope-Foley, AL", NA,
  #   $ AQS_Tribe.Name                   <chr> NA, NA, NA, NA, NA, NA, NA, NA,
  #   $ AQS_AQSID                        <chr> "010030010", "010270001", "01033

  # * fill in required columns -----

  meta$deviceType <- as.character(NA)
  meta$deviceDescription <- meta$AQS_Last.Method
  meta$deviceExtra <- as.character(NA)

  # pollutant
  if ( parameterCode == "44201" ) {
    meta$pollutant <- "OZONE"
    meta$units <- "PPM"
  } else if ( parameterCode == "42401" ) {
    meta$pollutant <- "SO2"
    meta$units <- "PPB"
  } else if ( parameterCode == "42101" ) {
    meta$pollutant <- "CO"
    meta$units <- "PPM"
  } else if ( parameterCode == "42602" ) {
    meta$pollutant <- "NO2"
    meta$units <- "PPB"
  } else if ( parameterCode == "88101" ) {
    meta$pollutant <- "PM2.5"
    meta$units <- "UG/M3"
  } else if ( parameterCode == "88502" ) {
    meta$pollutant <- "PM2.5"
    meta$units <- "UG/M3"
  } else if ( parameterCode == "81102" ) {
    meta$pollutant <- "PM10"
    meta$units <- "UG/M3"
  } else {
    meta$pollutant <- parameterCode
    meta$units <- as.character(NA)
  }

  meta$dataIngestSource <- "EPA_AQS"
  meta$dataIngestURL <- "https://aqs.epa.gov/aqsweb/airdata/download_files.html#Raw"
  meta$dataIngestUnitID <- as.character(NA)
  meta$dataIngestExtra <- as.character(NA)

  meta$dataIngestDescription <-
    sprintf("EPA AQS hourly data for parameterCode %s", parameterCode)

  # * add AQSID ----

  # NOTE:  The AQSID is particularly important as it identifies sites in the
  # NOTE:  AQS database and is used without POC in the AirNow data feeds as a
  # NOTE:  quasi-unique identifier for a timeseries at a location. AirNow may
  # NOTE:  switch among different POC devices but we don't have access to that
  # NOTE:  information in the AirNow data feed.

  meta$AQSID <- meta$AQS_AQSID

  # * remove unwanted columns -----

  removalColumns <- c(
    "AQS_Parameter.Code",
    "AQS_Parameter.Name",
    # "AQS_POC",
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
    "AQS_Site.Established.Date",
    "AQS_Site.Closed.Date",
    # "AQS_GMT.Offset",
    # "AQS_Owning.Agency",
    # "AQS_Address",
    # "AQS_CBSA.Name",
    "AQS_Tribe.Name",
    "AQS_AQSID"
  )

  meta <-
    meta %>%
    dplyr::select(-all_of(removalColumns))

  # ----- Return ---------------------------------------------------------------

    return(meta)

}

# ===== DEBUGGING ==============================================================

if ( FALSE ) {

  library(AirMonitorIngest)

  MazamaLocationUtils::setLocationDataDir("~/Data/monitoring/known_locations")
  sites_locationTbl <- MazamaLocationUtils::table_load("AQS_88101_sites")

  AQS_monitors <- epa_aqs_getMonitors(downloadDir = "~/Data/EPA")
  parameterCode <- "88101"

}

