#' @export
#' @import MazamaCoreUtils
#'
#' @title Create a meta dataframe with required data
#'
#' @description
#' Create a \code{meta} dataframe with monitor-specific metadata appropriate
#' for use with the \pkg{MazamaTimeSeries} package.
#'
#' \preformatted{
#' Core meta columns include:
#'
#'   deviceDeploymentID -- unique identifier (see MazmaLocationUtils)
#'   deviceID -- device identifier
#'   locationID -- location identifier (see MazmaLocationUtils)
#'   siteName -- English language name
#'   longitude -- decimal degrees E
#'   latitude -- decimal degrees N
#'   elevation -- elevation of station in m
#'   countryCode -- ISO 3166-1 alpha-2
#'   stateCode -- ISO 3166-2 alpha-2
#'   timezone -- Olson time zone
#'
#' Core data columns include:
#'
#'   datetime -- measurement time (UTC)
#' }
#'
#' @param locationTbl Table of "known locations" produced with \pkg{MazamaLocationUtils}.
#' @param monitorTbl Table of "monitors produced with \code{epa_aqs_getMonitors()}.
#'
#' @return Tibble enhanced with additional columns.
#'

epa_aqs_createMeta <- function(
  locationTbl = NULL,
  monitorTbl = NULL,
  parameterCode = NULL
) {


  if ( logger.isInitialized() )
    logger.debug(" ----- epa_aqs_createMeta() ----- ")

  # ----- Validate Parameters --------------------------------------------------

  MazamaLocationUtils::validateLocationTbl(locationTbl, locationOnly = FALSE)
  MazamaCoreUtils::stopIfNull(monitorTbl)

  # ----- Subset monitorTbl ----------------------------------------------------

  if ( !is.null(parameterCode) ) {
    monitorTbl <- dplyr::filter(monitorTbl, `Parameter Code` == parameterCode)
  }

  #   > dplyr::glimpse(locationTbl, width = 75)
  #   Rows: 1,862
  #   Columns: 24
  #   $ locationID                <chr> "91d2e5b1abd8dd01", "bcec1416572a724c",
  #   $ locationName              <chr> "Fairhope, Alabama", "Ashland", "Muscle
  #   $ longitude                 <dbl> -87.88026, -85.80361, -87.63810, -85.96
  #   $ latitude                  <dbl> 30.49748, 33.28493, 34.76262, 34.28857,
  #   $ elevation                 <dbl> 37.190, 344.420, 122.000, 500.000, 50.0
  #   $ countryCode               <chr> "US", "US", "US", "US", "US", "US", "US
  #   $ stateCode                 <chr> "AL", "AL", "AL", "AL", "AL", "AL", "AL
  #   $ county                    <chr> "Baldwin", "Clay", "Colbert", "DeKalb",
  #   $ timezone                  <chr> "America/Chicago", "America/Chicago", "
  #   $ houseNumber               <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
  #   $ street                    <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
  #   $ city                      <chr> "Fairhope", "Ashland", "Muscle Shoals",
  #   $ zip                       <chr> "36532", "36251", "35661", "35962", "36
  #   $ AQS_Site.Number           <chr> "0010", "0001", "1002", "1003", "0002",
  #   $ AQS_Site.Established.Date <chr> "2000-01-01", "1991-03-25", "1977-01-01
  #   $ AQS_Site.Closed.Date      <chr> NA, NA, "2019-11-01", NA, "2007-12-30",
  #   $ AQS_GMT.Offset            <dbl> -6, -6, -6, -6, -6, -6, -6, -6, -6, -6,
  #   $ AQS_Owning.Agency         <chr> "Al Dept Of Env Mgt", "Al Dept Of Env M
  #   $ AQS_Local.Site.Name       <chr> "FAIRHOPE, Alabama", "ASHLAND", "MUSCLE
  #   $ AQS_Address               <chr> "FAIRHOPE HIGH SCHOOL, 1 PIRATE DRIVE,
  #   $ AQS_CBSA.Name             <chr> "Daphne-Fairhope-Foley, AL", NA, "Flore
  #   $ AQS_Tribe.Name            <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
  #   $ AQS_Extraction.Date       <chr> "2021-05-18", "2021-05-18", "2021-05-18
  #   $ AQS_AQSID                 <chr> "010030010", "010270001", "010331002",

  #   > dplyr::glimpse(monitorTbl, width = 75)
  #   Rows: 3,523
  #   Columns: 32
  #   $ `State Code`                   <chr> "01", "01", "01", "01", "01", "01"
  #   $ `County Code`                  <chr> "003", "027", "033", "033", "049",
  #   $ `Site Number`                  <chr> "0010", "0001", "1002", "1002", "1
  #   $ `Parameter Code`               <chr> "88101", "88101", "88101", "88101"
  #   $ `Parameter Name`               <chr> "PM2.5 - Local Conditions", "PM2.5
  #   $ POC                            <chr> "1", "1", "1", "3", "1", "1", "1",
  #   $ Latitude                       <dbl> 30.49748, 33.28493, 34.76262, 34.7
  #   $ Longitude                      <dbl> -87.88026, -85.80361, -87.63810, -
  #   $ Datum                          <chr> "NAD83", "NAD83", "NAD83", "NAD83"
  #   $ `First Year of Data`           <dbl> 2000, 1999, 1999, 2009, 1999, 2000
  #   $ `Last Sample Date`             <chr> "2020-12-29", "2020-12-29", "2019-
  #   $ `Monitor Type`                 <chr> "SLAMS", "SLAMS", "SLAMS", "SLAMS"
  #   $ Networks                       <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA
  #   $ `Reporting Agency`             <chr> "Al Dept Of Env Mgt", "Al Dept Of
  #   $ PQAO                           <chr> "Al Dept Of Env Mgt", "Al Dept Of
  #   $ `Collecting Agency`            <chr> "Al Dept Of Env Mgt", "Al Dept Of
  #   $ Exclusions                     <chr> NA, NA, NA, "All (2009-04-03 - 201
  #   $ `Monitoring Objective`         <chr> "POPULATION EXPOSURE", "REGIONAL T
  #   $ `Last Method Code`             <chr> "145", "145", "145", "170", "145",
  #   $ `Last Method`                  <chr> "R & P Model 2025 PM-2.5 Sequentia
  #   $ `Measurement Scale`            <chr> "NEIGHBORHOOD", "REGIONAL SCALE",
  #   $ `Measurement Scale Definition` <chr> "500 M TO 4KM", "50 TO HUNDREDS KM
  #   $ `NAAQS Primary Monitor`        <chr> "Y", "Y", NA, NA, "Y", NA, "Y", NA
  #   $ `QA Primary Monitor`           <chr> NA, NA, NA, NA, NA, NA, "Y", NA, N
  #   $ `Local Site Name`              <chr> "FAIRHOPE, Alabama", "ASHLAND", "M
  #   $ Address                        <chr> "FAIRHOPE HIGH SCHOOL, 1 PIRATE DR
  #   $ `State Name`                   <chr> "Alabama", "Alabama", "Alabama", "
  #   $ `County Name`                  <chr> "Baldwin", "Clay", "Colbert", "Col
  #   $ `City Name`                    <chr> "Fairhope", "Ashland", "Muscle Sho
  #   $ `CBSA Name`                    <chr> "Daphne-Fairhope-Foley, AL", NA, "
  #   $ `Tribe Name`                   <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA
  #   $ `Extraction Date`              <chr> "2021-05-18", "2021-05-18", "2021-


  # TODO:  Find known locations

  # > a <- table_getLocationID(locationTbl, monitorTbl$Longitude, monitorTbl$Latitude, distanceThreshold = 100)
  # > anyNA(a)
  # [1] TRUE

  # TODO:  Reduce monitor columns

  # TODO:  Join based on locationID

  # TIDIL  Add extra PWFSL metadata



  # The PWFSLSmoke v1.0 data model contains the following parameters
  #
  # > names(meta)
  #  [1] "monitorID"             "longitude"             "latitude"              "elevation"
  #  [5] "timezone"              "countryCode"           "stateCode"             "siteName"
  #  [9] "agencyName"            "countyName"            "msaName"               "monitorType"
  # [13] "siteID"                "instrumentID"          "aqsID"                 "pwfslID"
  # [17] "pwfslDataIngestSource" "telemetryAggregator"   "telemetryUnitID"




  meta <- locationTbl



  return(meta)

}
