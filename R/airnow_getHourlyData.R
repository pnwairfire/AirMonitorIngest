#' @export
#' @importFrom rlang .data
#'
#' @title Download and aggregate multiple hourly data files from AirNow
#'
#' @param parameterName One or more EPA AQS criteria parameter names.
#' @param starttime Desired start datetime (ISO 8601).
#' @param endtime Desired end datetime (ISO 8601).
#' @param timezone Olson timezone used to interpret dates (required).
#' @param baseUrl Base URL for archived hourly data.
#' @param verbose Logical flag controlling detailed progress statements.
#'
#' @return Tibble of AirNow hourly data.
#'
#' @description This function downloads, parses and combines hourly data files
#' from \code{baseUrl}.
#'
#' Parameters included in AirNow data include at least the following list:
#'
#' \enumerate{
#' \item{BARPR}
#' \item{BC}
#' \item{CO}
#' \item{NO}
#' \item{NO2}
#' \item{NO2Y}
#' \item{NO2X}
#' \item{NOX}
#' \item{NOOY}
#' \item{OC}
#' \item{OZONE}
#' \item{PM10}
#' \item{PM2.5}
#' \item{PRECIP}
#' \item{RHUM}
#' \item{SO2}
#' \item{SRAD}
#' \item{TEMP}
#' \item{UV-AETH}
#' \item{WD}
#' \item{WS}
#' }
#'
#' Passing a vector of one ore more of the above names as the \code{parameters}
#' argument will cause the resulting tibble to be filtered to contain only
#' records for those parameters.
#'
#' Datetimes can be anything that is understood by
#' \code{MazamaCoreUtils::parseDatetime()}. For non-POSIXct values, the
#' recommended format is "YYYY-mm-dd HH:00:00" or just "YYYYmmddhh".
#'
#' @note Hourly data are available from AirNow starting on 2014-01-01.
#'
#' @note Data from locations whose timezones have a fractional offset from UTC
#' are removed as the \emph{mts_monitor} data model only supports data reported
#' on hour boundaries. As of 2019-06-26, fractional offsets only applies to US
#' Department of State monitors in Myanmar, Sri Lanka, India and Nepal.
#'
#' @examples
#' \dontrun{
#' # Fail gracefully if any resources are not available
#' try({
#'
#' library(AirMonitorIngest)
#'
#' tbl <-
#'   airnow_getHourlyData(
#'     parameterName = "PM2.5",
#'     starttime = 2021101200,
#'     endtime = 2021101204,
#'     timezone = "America/Los_Angeles",
#'     verbose = TRUE
#'    )
#'
#' }, silent = FALSE)
#' }

airnow_getHourlyData <- function(
  parameterName = c("PM2.5"), ###, "CO", "OZONE", "PM10"),
  starttime = NULL,
  endtime = NULL,
  timezone = "UTC",
  baseUrl = 'https://files.airnowtech.org/airnow',
  verbose = FALSE
) {

  if ( FALSE ) {

    parameterName = "PM2.5"
    starttime = 2021101200
    endtime = 2021101300
    timezone = "America/Los_Angeles"
    baseUrl = 'https://files.airnowtech.org/airnow'

  }




  if ( MazamaCoreUtils::logger.isInitialized() )
    logger.debug(" ----- airnow_getHourlyData() ----- ")

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(starttime)
  MazamaCoreUtils::stopIfNull(endtime)
  MazamaCoreUtils::stopIfNull(timezone)

  parameterName <- match.arg(parameterName, several.ok = TRUE)

  baseUrl <- MazamaCoreUtils::setIfNull(baseUrl, "https://files.airnowtech.org/airnow")

  if ( !timezone %in% base::OlsonNames() )
    stop(sprintf("'timezone = %s' is not found in OlsonNames()", timezone))

  starttime <- MazamaCoreUtils::parseDatetime(starttime, timezone = timezone)
  endtime <- MazamaCoreUtils::parseDatetime(endtime, timezone = timezone)

  datestamps <-
    seq(starttime, endtime, by = "hours") %>%
    MazamaCoreUtils::timeStamp(timezone = "UTC", unit = "hour")

  # ----- Get data -------------------------------------------------------------

  # Pre-allocate an empty list of the appropriate length
  tblList <- vector(mode = "list", length = length(datestamps))

  for ( datestamp in datestamps ) {

    # Obtain an hour of AirNow data
    result <- try({
      tblList[[datestamp]] <- .airnow_downloadHourlyData(
        parameterName = parameterName,
        datetime = datestamp,
        baseUrl = baseUrl,
        verbose = verbose
      )
    }, silent = TRUE)

    if ( "try-error" %in% class(result) ) {
      err_msg <- stringr::str_trim(geterrmessage())
      if ( MazamaCoreUtils::logger.isInitialized() )
        logger.warn("Unable to download data: %s",err_msg)
      next
    }

  }

  # Combine all tibbles, removing duplicates
  tbl <-
    dplyr::bind_rows(tblList) %>%
    dplyr::distinct()

  # ----- Return ---------------------------------------------------------------

  return(tbl)

}


# ===== Internal Functions =====================================================

# @note Data from locations whose timezones have a fractional offset from UTC
# are removed as the mts_monitor data model only supports data reported on hour
# boundaries. As of 2019-06-26, fractional offsets only applies to US Department
# of State monitors in Myanmar, Sri Lanka, India and Nepal.

.airnow_downloadHourlyData <- function(
  parameterName = NULL,
  datetime = NULL,
  baseUrl = 'https://files.airnowtech.org/airnow',
  verbose = FALSE
) {

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(parameterName)
  MazamaCoreUtils::stopIfNull(datetime)

  # Strip off any final '/'
  baseUrl <- stringr::str_replace(baseUrl, '/$', '')

  # Create URL
  datestamp <- MazamaCoreUtils::timeStamp(datetime, "UTC", unit = "hour")
  year <- stringr::str_sub(datestamp, 1, 4)
  ymd <- stringr::str_sub(datestamp, 1, 8)
  url <- paste0(baseUrl, '/', year, '/', ymd, '/HourlyData_', datestamp, '.dat')

  if ( verbose )
    message(sprintf("Downloading %s ...", url))

  if ( MazamaCoreUtils::logger.isInitialized() )
    logger.trace("Downloading %s ...", url)

  # ----- Download/parse data file ---------------------------------------------

  col_names <- c(
    'ValidDate', 'ValidTime', 'AQSID', 'siteName', 'GMTOffset',
    'parameterName', 'parameterUnits', 'parameterConcentration', 'agencyName'
  )

  col_types <- 'ccccdccdc'

  # NOTE:  Encoding issues:
  # NOTE:
  # NOTE:  12/26/16|00:00|000051501|Z<82>phirin|-5|OZONE|PPB|39|Meteorological Service of Canada
  locale <- readr::locale(encoding = "CP437")

  # Read in text as a dataframe
  tbl <-

    readr::read_delim(
      url,
      delim = '|',
      col_names = col_names,
      col_types = col_types,
      locale = locale,
      num_threads = 1,
      progress = FALSE,
      show_col_types = FALSE
      # Use default settings for everything else
    ) %>%

    # Remove records with fractional hours (see @note above)
    dplyr::filter(stringr::str_detect(.data$ValidTime, ":00")) %>%

    # Subset to parameter of interest
    dplyr::filter(parameterName == !!parameterName) %>%

    # Create utcTime
    dplyr::mutate(
      utcTime = lubridate::mdy_hm(paste(.data$ValidDate, .data$ValidTime))
    ) %>%

    # Clean up
    dplyr::select(c(
      "utcTime", "parameterName", "parameterConcentration", "parameterUnits",
      "siteName", "agencyName", "AQSID"
    ))

  # > dplyr::glimpse(tbl, width = 75)
  # Rows: 997
  # Columns: 7
  # $ utcTime                <dttm> 2018-09-05 22:00:00, 2018-09-05 22:00:00,
  # $ parameterName          <chr> "PM2.5", "PM2.5", "PM2.5", "PM2.5", "PM2.5
  # $ parameterConcentration <dbl> 2, 7, 3, 10, -1, 4, 22, 1, 3, 7, 14, 7, 11
  # $ parameterUnits         <chr> "UG/M3", "UG/M3", "UG/M3", "UG/M3", "UG/M3
  # $ siteName               <chr> "Mount Pearl", "MacPherson Avenue - ", "Ma
  # $ agencyName             <chr> "Newfoundland & Labrador DEC", "Environmen
  # $ AQSID                  <chr> "000010401", "000010602", "000010901", "00


  # NOTE:  Below is what we get back from airnow_getData()
  #
  # > dplyr::glimpse(tbl, width = 75)
  # Rows: 1,114
  # Columns: 13
  # $ latitude                  <dbl> 21.32360, 21.39283, 21.30390, 21.31030,
  # $ longitude                 <dbl> -158.0886, -157.9691, -157.8711, -157.8
  # $ utcTime                   <dttm> 2021-10-18 09:00:00, 2021-10-18 09:00:
  # $ parameterName             <chr> "PM2.5", "PM2.5", "PM2.5", "PM2.5", "PM
  # $ parameterConcentration    <dbl> 2.3, 2.8, 3.4, 1.9, 1.1, 5.6, 3.0, 8.2,
  # $ parameterUnits            <chr> "UG/M3", "UG/M3", "UG/M3", "UG/M3", "UG
  # $ parameterRawConcentration <dbl> 2.00000, 3.00000, 4.00000, 1.00000, 1.0
  # $ parameterAQI              <dbl> 10, 12, 14, 8, 5, 23, 13, 34, 15, 10, 5
  # $ parameterAQC              <chr> "1", "1", "1", "1", "1", "1", "1", "1",
  # $ siteName                  <chr> "Kapolei", "Pearl City", "SAND ISLAND 2
  # $ agencyName                <chr> "Hawaii State Dept. of Health", "Hawaii
  # $ AQSID                     <chr> "150030010", "150032004", "150031004",
  # $ fullAQSID                 <chr> "840150030010", "840150032004", "840150

  # ----- Return ---------------------------------------------------------------

  return(tbl)

}
