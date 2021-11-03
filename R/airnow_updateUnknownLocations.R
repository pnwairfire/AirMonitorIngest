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

  #   > dplyr::glimpse(locationTbl, width=75)
  #   Rows: 1,617
  #   Columns: 23
  #   $ locationID            <chr> "57bc483da6023722", "86726ac7458c6f20", "9e
  #   $ locationName          <chr> "Haverhill", "Hinds Cc", "Saint-Simon", "Ma
  #   $ longitude             <dbl> -71.10280, -90.22593, -73.46860, -77.40027,
  #   $ latitude              <dbl> 42.77080, 32.34690, 45.44310, 37.55652, 47.
  #   $ elevation             <dbl> 0.0, 0.0, 40.3, 58.6, 79.3, 0.0, 0.0, 80.8,
  #   $ countryCode           <chr> "US", "US", "CA", "US", "US", "CA", "US", "
  #   $ stateCode             <chr> "MA", "MS", "QC", "VA", "WA", "BC", "OR", "
  #   $ countyName            <chr> "Essex", "Hinds", NA, "Henrico", "Chelan",
  #   $ timezone              <chr> "America/New_York", "America/Chicago", "Ame
  #   $ houseNumber           <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
  #   $ street                <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
  #   $ city                  <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
  #   $ zip                   <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
  #   $ AQSID                 <chr> "250095005", "280490021", "000052201", "510
  #   $ airnow_parameterName  <chr> "PM2.5", "PM2.5", "PM2.5", "PM2.5", "PM2.5"
  #   $ airnow_siteCode       <chr> "5005", "0021", "2201", "0014", "0011", "55
  #   $ airnow_status         <chr> "Active", "Active", "Active", "Active", "Ac
  #   $ airnow_agencyID       <chr> "MA1", "MS1", "QC1", "VA1", "WA1", "BC1", "
  #   $ airnow_agencyName     <chr> "Massachusetts Dept. of Environmental Prote
  #   $ airnow_EPARegion      <chr> "R1", "R4", "CA", "R3", "R10", "CA", "R10",
  #   $ airnow_GMTOffsetHours <dbl> -5, -6, -5, -5, -8, -8, -8, -8, -5, -8, -8,
  #   $ airnow_FIPSMSACode    <chr> "14460", "27140", NA, "40060", NA, NA, "389
  #   $ airnow_MSAName        <chr> " Boston-Cambridge-Quincy, MA-NH ", " Jacks

  # NOTE:  This step is not easy with dplyr

  # Replace locationName with siteName where siteName is defined
  mask <- !is.na(new_locationTbl$siteName)
  new_locationTbl$locationName[mask] <- new_locationTbl$siteName[mask]

  unwantedColumns <- setdiff(names(new_locationTbl), names(locationTbl))

  new_locationTbl <-

    new_locationTbl %>%

    # Add required columns
    dplyr::mutate(
      airnow_parameterName = .data$parameterName,
      airnow_siteCode = as.character(NA),
      airnow_status = "Active",
      airnow_agencyID = as.character(NA),
      airnow_agencyName = .data$agencyName,
      airnow_EPARegion = as.character(NA),
      airnow_GMTOffsetHours = as.numeric(NA),
      airnow_FIPSMSACode = as.character(NA),
      airnow_MSAName = as.character(NA)
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
