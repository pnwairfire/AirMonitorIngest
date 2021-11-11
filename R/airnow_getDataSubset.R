#' @export
#' @importFrom utils read.table
#' @importFrom rlang .data
#'
#' @title Download and parse hourly data from the AirNow data API
#'
#' @param parameterName One or more EPA AQS criteria parameter names.
#' @param starttime Desired start datetime (ISO 8601).
#' @param endtime Desired end datetime (ISO 8601).
#' @param timezone Olson timezone used to interpret dates (required).
#' @param monitorType Subset of all monitors to select.
#' @param baseUrl Base URL for archived hourly data.
#'
#' @return Tibble of AirNow hourly data.
#'
#' @description This function uses the AirNow data webservice to retrieve
#' subsets of data that do not exceed a maximum data size which causes
#' errors.
#'
#' Datetimes can be anything that is understood by
#' \code{MazamaCoreUtils::parseDatetime()}. For non-POSIXct values, the
#' recommended format is "YYYY-mm-dd HH:00:00" or just "YYYYmmddhh".
#'

airnow_getDataSubset <- function(
  parameterName = c("PM2.5"), ###, "CO", "OZONE", "PM10"),
  starttime = NULL,
  endtime = NULL,
  timezone = "UTC",
  monitorType = c("both", "permanent", "mobile"),
  baseUrl = "https://www.airnowapi.org/aq/data/"
) {

  if ( MazamaCoreUtils::logger.isInitialized() )
    logger.debug(" ----- airnow_getDataSubset() ----- ")

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(starttime)
  MazamaCoreUtils::stopIfNull(endtime)
  MazamaCoreUtils::stopIfNull(timezone)

  parameterName <- match.arg(parameterName, several.ok = TRUE)
  monitorType <- match.arg(monitorType, several.ok = FALSE)

  MazamaCoreUtils::setIfNull(baseUrl, "https://www.airnowapi.org/aq/data/")

  if ( !timezone %in% base::OlsonNames() )
    stop(sprintf("'timezone = %s' is not found in OlsonNames()", timezone))

  AIRNOW_API_KEY <- getAPIKey("airnow")
  if ( is.null(AIRNOW_API_KEY) )
    stop(paste0(
      "The AirNow API_KEY is not set. Please set it with:\n\n",
      "  setAPIKey(\"airnow\", <AIRNOW_API_KEY>)"
    ))

  # ----- Prepare request ------------------------------------------------------

  # bbox (w,s,e,n)

  # > library(MazamaSpatialUtils)
  # > setSpatialDataDir("~/Data/Spatial")
  # > loadSpatialDdata("TMWorldBorders_02)
  # > subset(TMWorldBorders_02, countryCode %in% c("US", "CA", "MX", "PR", "VI")) %>% bbox()
  # min       max
  # x -167.86331 -52.61445
  # y   14.55055  83.10942

  # TODO:  support bbox as a function parameter

  bbox <- "-170.0,15.0,-50.0,85.0"


  # Let's be hyper-explicit about the times we are talking about
  timeRange <-
    MazamaCoreUtils::timeRange(
      starttime = starttime,
      endtime = endtime,
      timezone = timezone,
      unit = "hour",
      ceilingStart = FALSE,
      ceilingEnd = FALSE
    )

  # Timestamps
  timeStamp <-
    MazamaCoreUtils::timeStamp(
      timeRange,
      timezone = "UTC",
      unit = "hour",
      style = "clock"
    )

  # Comma-separated "parameters"
  parameters <-
    parameterName %>%
    stringr::str_replace("CO", "o3") %>%
    stringr::str_replace("NO2", "no2") %>%
    stringr::str_replace("OZONE", "o3") %>%
    stringr::str_replace("PM2.5", "pm25") %>%
    stringr::str_replace("PM10", "pm10") %>%
    stringr::str_replace("SO2", "so2") %>%
    paste0(collapse = ",")


  # monitortype

  monitortype <-
    switch(
      monitorType,
      permanent = 0,
      mobile = 1,
      both = 2
    )

  # ----- Guess at max size ----------------------------------------------------

  # NOTE:  It appears I can get 12 hours of permament monitoring data for PM2.5.

  hourCount <-
    difftime(timeRange[2], timeRange[1], units = "hours") %>%
    as.numeric() + 1

  if ( (hourCount * length(parameterName)) > 12 )
    stop(paste0(
      "too much data requested.\n",
      "Please reduce the time range or the number of parameterNames."
    ))

  # ----- Request parameters ---------------------------------------------------

  # NOTE:  See the document describing the REST API:
  # NOTE:    https://docs.airnowapi.org/Data/docs

  # https://www.airnowapi.org/aq/data/?\
  #   startdate=2014-09-23t22&\
  #   enddate=2014-09-23t23&\
  #   parameters=o3,pm25&\
  #   bbox=-90.806632,24.634217,-71.119132,45.910790&\
  #   datatype=a&\
  #   format=application/vnd.google-earth.kml&\
  #   api_key=8B9D8258-1A36-463F-A447-ACB284AD6CDC

  # NOTE:  We hardcode certain options:
  # NOTE:  datatype = "B"      -- always get both concentration and AQI values
  # NOTE:  format = "text/csv" -- always get the most compact format
  # NOTE:  verbose = "1" -- always get site metadata
  # NOTE:  includeraw... = "1" -- always get raw concentrations

  # Create URL parameters
  .params <- list(
    bbox = bbox,
    startdate = timeStamp[1],
    enddate = timeStamp[2],
    parameters = parameters,
    monitortype = monitortype,
    datatype = "B",
    format = "text/csv",
    api_key = AIRNOW_API_KEY,
    verbose = "1",
    includerawconcentrations = "1"
  )

  # ----- Download data --------------------------------------------------------

  if ( MazamaCoreUtils::logger.isInitialized() )
    logger.trace("Downloading AirNow data ...")

  suppressWarnings({
    r <- httr::GET(baseUrl, query = .params)
  })

  # NOTE:  Log the error but just return an empty string (aka "No data")
  # NOTE:  for downstream processing.
  if ( httr::http_error(r) ) {
    if ( MazamaCoreUtils::logger.isInitialized() ) {
      logger.error("AirNow API data service failed with: %s", httr::content(r))
    }
    return("") # Return a blank fileString
  }

  # No error so return the content (which might be an HTML formatted error message)
  fileString <- httr::content(r, 'text', encoding = 'UTF-8')

  if ( stringr::str_detect(fileString, "AirNow ERROR MESSAGE TO DETECT") ) {
    stop(paste0(
      "ERROR MESSAGE GOES HERE???"
    ))
  }

  if ( MazamaCoreUtils::logger.isInitialized() )
    logger.trace("Finished downloading AirNow data.")

  # ----- Parse data -----------------------------------------------------------

  # NOTE:  Full description of the data format at:
  # NOTE:    https://docs.airnowapi.org/Data/docs

  ### NOTE:  Opportunity to filter out bad lines
  ###
  ### lines <- readr::read_lines(fileString)
  ### # Figure out goodLines mask
  ### fakeFile <- paste0(lines[goodLines], collapse = "\n")

  fakeFile <- fileString

  columnNames <- c(
    "latitude",
    "longitude",
    "utcTime",
    "parameterName",
    "parameterConcentration", # NowCast for Ozone, PM2.5 and PM10
    "parameterUnits",
    "parameterRawConcentration",
    "parameterAQI",
    "parameterAQC",
    "siteName",
    "agencyName",
    "AQSID",
    "fullAQSID"
  )

  columnTypes <- "ddTcdcddcccc"

  tbl <-
    readr::read_csv(
      fakeFile,
      col_names = columnNames,
      col_types = columnTypes
    ) %>%
    # They are using -999 as a numeric missing value flag and "N/A" for characters
    dplyr::mutate(
      parameterConcentration = dplyr::na_if(.data$parameterConcentration, -999),
      parameterRawConcentration = dplyr::na_if(.data$parameterRawConcentration, -999),
      parameterAQI = dplyr::na_if(.data$parameterAQI, -999),
      siteName = dplyr::na_if(.data$siteName, "N/A"),
      agencyName = dplyr::na_if(.data$agencyName, "N/A")
    )



  # ----- Return ---------------------------------------------------------------

  return(tbl)

}

# ===== DEBUGGING ==============================================================

if ( FALSE ) {


  library(MazamaCoreUtils)
  logger.setLevel(TRACE)

  library(AirMonitorIngest)

  starttime <- 2021102402
  endtime <- 2021102402
  timezone <- "America/Los_Angeles"
  parameterName <- "PM2.5"
  monitorType <- "permanent"
  baseUrl <- "https://www.airnowapi.org/aq/data/"

  tbl <-
    airnow_getDataSubset(
      starttime = starttime,
      endtime = endtime,
      timezone = timezone,
      parameterName = parameterName,
      monitorType = monitorType
    )




  MazamaLocationUtils::table_leaflet(
    tbl,
    maptype = "terrain",
    extraVars = c("siteName", "agencyName", "AQSID"),
    weight = 1
  )

}
