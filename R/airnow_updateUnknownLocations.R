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
#' @param airnow_data Table of monitor data obtained with \code{airnow_getData()}.
#'
#' @return Tibble of device-deployment metadata.
#'

airnow_updateUnknownLocations <- function(
  locationTbl = NULL,
  distanceThreshold = NULL,
  airnow_data = NULL
) {

  if ( logger.isInitialized() )
    logger.debug(" ----- airnow_updateUnknownLocations() ----- ")

  # ----- Validate Parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(locationTbl)
  MazamaCoreUtils::stopIfNull(distanceThreshold)
  MazamaCoreUtils::stopIfNull(airnow_data)

  # Find individual locations assuming last-is-best
  airnow_data_locations <-
    airnow_data %>%
    dplyr::arrange(dplyr::desc(.data$utcTime)) %>%
    dplyr::distinct(.data$longitude, .data$latitude, .keep_all = TRUE)

  # ----- Identify new locations -----------------------------------------------

  # NOTE:  Do this to be sure we don't overwrite existing known locations.

  airnow_unknown <-

    airnow_data_locations %>%

    # Add locationID for "known locations"
    dplyr::mutate(
      locationID = MazamaLocationUtils::table_getLocationID(
        locationTbl = !!locationTbl,
        longitude = .data$longitude,
        latitude = .data$latitude,
        distanceThreshold = !!distanceThreshold,
        measure = "geodesic"
      )
    ) %>%

    # Filter for "unknown locations"
    dplyr::filter(is.na(.data$locationID))

  # ----- Create new "known_locations" records ---------------------------------

  new_locationTbl <-
    MazamaLocationUtils::table_initializeExisting(
      airnow_unknown,
      countryCodes = c("CA", "US", "MX", "PR", "VI", "GU"),
      distanceThreshold = distanceThreshold,
      measure = "geodesic",
      verbose = FALSE
    )

  # NOTE:  This is what we have:

  #   > dplyr::glimpse(new_locationTbl, width = 75)
  #   Rows: 3
  #   Columns: 24
  #   $ locationID                <chr> "3047c3852b3b0037", "8e8530836c4b45f5",
  #   $ locationName              <chr> "us.ca_3047c3", "us.ca_8e8530", "us.nm_
  #   $ longitude                 <dbl> -121.9518, -119.7435, -105.8956
  #   $ latitude                  <dbl> 41.26162, 37.46455, 35.68705
  #   $ elevation                 <dbl> NA, NA, NA
  #   $ countryCode               <chr> "US", "US", "US"
  #   $ stateCode                 <chr> "CA", "CA", "NM"
  #   $ countyName                <chr> NA, NA, NA
  #   $ timezone                  <chr> "America/Los_Angeles", "America/Los_Ang
  #   $ houseNumber               <chr> NA, NA, NA
  #   $ street                    <chr> NA, NA, NA
  #   $ city                      <chr> NA, NA, NA
  #   $ zip                       <chr> NA, NA, NA
  #   $ utcTime                   <dttm> 2021-11-02 22:00:00, 2021-11-02 22:00:0
  #   $ parameterName             <chr> "PM2.5", "PM2.5", "PM2.5"
  #   $ parameterConcentration    <dbl> 0.0, 4.1, 30.7
  #   $ parameterUnits            <chr> "UG/M3", "UG/M3", "UG/M3"
  #   $ parameterRawConcentration <dbl> NA, NA, 39
  #   $ parameterAQI              <dbl> 0, 17, 90
  #   $ parameterAQC              <chr> "1", "1", "2"
  #   $ siteName                  <chr> NA, NA, "Unit386"
  #   $ agencyName                <chr> "California Air Resources Board", "Cali
  #   $ AQSID                     <chr> "840MMCA82040", "840MMCA82036", "MMFS10
  #   $ fullAQSID                 <chr> "840MMCA82040", "840MMCA82036", "840MMF


  # NOTE:  And this is what we want to have:

  # > dplyr::glimpse(locationTbl, width = 75)
  # Rows: 2,137
  # Columns: 31
  # $ locationID            <chr> "ff93e44a39b72bc9", "f6b4431a3529dad4", "10
  # $ AQSID                 <chr> "480290055", "311090022", "080350004", "840
  # $ locationName          <chr> "CPS Pecan Valley C678", "LLCHD BAM", "Chat
  # $ longitude             <dbl> -98.43110, -96.67573, -105.07000, -98.37889
  # $ latitude              <dbl> 29.40720, 40.81256, 39.53390, 40.91833, 29.
  # $ elevation             <dbl> 189.10, 374.19, 1571.10, 573.10, 9.20, 1339
  # $ countryCode           <chr> "US", "US", "US", "US", "US", "US", "CA", "
  # $ stateCode             <chr> "TX", "NE", "CO", "NE", "TX", "UT", "AB", "
  # $ countyName            <chr> "Bexar", "Lancaster", "Douglas", "Hall", "H
  # $ timezone              <chr> "America/Chicago", "America/Chicago", "Amer
  # $ houseNumber           <chr> "973", "3159", NA, "208", "7372", "2501", N
  # $ street                <chr> "H Street", "N Street", "Chatfield Internal
  # $ city                  <chr> "San Antonio", "Lincoln", NA, "Grand Island
  # $ zip                   <chr> "78220", "68510", "80125", "68803", "77087"
  # $ airnow_parameterName  <chr> "PM2.5", "PM2.5", "PM2.5", "PM2.5", "PM2.5"
  # $ airnow_siteCode       <chr> "0055", "0022", "0004", "0790", "0416", "10
  # $ airnow_status         <chr> "Inactive", "Active", "Active", "Active", "
  # $ airnow_agencyID       <chr> "TX1", "NE4", "CO1", "NE1", "TX1", "UT1", "
  # $ airnow_agencyName     <chr> "Texas Commission on Environmental Quality"
  # $ airnow_EPARegion      <chr> "R6", "R7", "R8", "R7", "R6", "R8", "CA", "
  # $ airnow_GMTOffsetHours <dbl> -6, -6, -7, -6, -6, -7, -7, -6, -8, -5, -6,
  # $ airnow_FIPSMSACode    <chr> "41700", "30700", "19740", "24260", "26420"
  # $ airnow_MSAName        <chr> "San Antonio, TX", "Lincoln, NE", "Denver-A
  # $ address               <chr> "973 H Street, San Antonio, TX 78220, Unite
  # $ airnow_stationID      <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
  # $ airnow_fullAQSID      <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
  # $ airnow_monitorType    <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
  # $ airnow_CBSA_ID        <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
  # $ airnow_CBSA_Name      <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
  # $ airnow_stateAQSCode   <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
  # $ airnow_countyAQSCode  <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,

  # NOTE:  This step is not easy with dplyr

  # Replace locationName with siteName where siteName is defined
  mask <- !is.na(new_locationTbl$siteName)
  new_locationTbl$locationName[mask] <- new_locationTbl$siteName[mask]

  unwantedColumns <- setdiff(names(new_locationTbl), names(locationTbl))

  new_locationTbl <-

    new_locationTbl %>%

    # Add spatial metadata
    dplyr::mutate(
      airnow_parameterName = .data$parameterName,
      airnow_siteCode = as.character(NA),
      airnow_status = "Active",
      airnow_agencyID = as.character(NA),
      airnow_agencyName = .data$agencyName,
      airnow_EPARegion = as.character(NA),
      airnow_GMTOffsetHours = as.numeric(NA),
      airnow_FIPSMSACode = as.character(NA),
      airnow_MSAName = as.character(NA),
      airnow_stationID = as.character(NA),
      airnow_fullAQSID = .data$fullAQSID,
      airnow_monitorType = as.character(NA),
      airnow_CBSA_ID = as.character(NA),
      airnow_CBSA_Name = as.character(NA),
      airnow_stateAQSCode = as.character(NA),
      airnow_countyAQSCode = as.character(NA)
    ) %>%

    # Remove unwanted columns
    dplyr::select(-dplyr::all_of(unwantedColumns))

  # ----- Return ---------------------------------------------------------------

  locationTbl <- dplyr::bind_rows(locationTbl, new_locationTbl)

  return(locationTbl)

}

# ===== DEBUGGING ==============================================================

if ( FALSE ) {



}
