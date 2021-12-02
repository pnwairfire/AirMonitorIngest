#' @export
#' @importFrom MazamaCoreUtils logger.trace logger.debug logger.warn logger.error
#'
#' @title Download AIRSIS data
#'
#' @param startdate Desired start date (ISO 8601).
#' @param enddate Desired end date (ISO 8601).
#' @param timezone Olson timezone used to interpret dates (required).
#' @param provider Identifier used to modify baseURL \code{[APCD|USFS|...]}
#' @param unitID AIRSIS station identifier (will be upcased).
#' @param baseUrl Base URL for data queries.
#'
#' @description Download data for a particular station for the desired time period.
#' Data are returned as a single character string containing the AIRIS output.
#'
#' @return String containing AIRSIS output.
#'

airsis_downloadData <- function(
  startdate = strftime(lubridate::now(tzone = "UTC"), "%Y0101", tz = "UTC"),
  enddate = strftime(lubridate::now(tzone = "UTC"), "%Y%m%d", tz = "UTC"),
  timezone = "UTC",
  provider = NULL,
  unitID = NULL,
  baseUrl = "http://xxx.airsis.com/vision/common/CSVExport.aspx"
) {

  logger.debug(" ----- airsis_downloadData() ----- ")

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(startdate)
  MazamaCoreUtils::stopIfNull(enddate)
  MazamaCoreUtils::stopIfNull(timezone)
  MazamaCoreUtils::stopIfNull(provider)
  MazamaCoreUtils::stopIfNull(unitID)

  # Get UTC times
  starttime <- MazamaCoreUtils::parseDatetime(startdate, timezone = "UTC")
  endtime <- MazamaCoreUtils::parseDatetime(enddate, timezone = "UTC")

  # ----- Download data --------------------------------------------------------

  logger.trace("Downloading AIRSIS data for unitID %s ...", unitID)

  # Example URL:
  #   http://usfs.airsis.com/vision/common/CSVExport.aspx?uid=1026&StartDate=2016-02-03&EndDate=2016-02-03

  # Create a valid baseUrl
  baseUrl <- stringr::str_replace(baseUrl, 'xxx', tolower(provider))

  query <- list(
    uid = unitID,
    StartDate = strftime(starttime, "%Y-%m-%d", tz = "UTC"),
    EndDate = strftime(endtime, "%Y-%m-%d", tz = "UTC")
  )

  suppressWarnings({
    r <- httr::GET(baseUrl, query = query)
  })

  if ( httr::http_error(r) ) {
    logger.error("WRCC data service failed for: %s", r$url)
    logger.error("WRCC data service failed with: %s", httr::content(r))
    return("")
  }

  fileString <- httr::content(r, 'text', encoding = 'UTF-8')

  # ----- Return ---------------------------------------------------------------

  return(fileString)

}

# ===== DEBUGGING ==============================================================

if ( FALSE ) {

  startdate = MazamaCoreUtils::parseDatetime("2021-11-01", timezone = "UTC")
  enddate = MazamaCoreUtils::parseDatetime("2021-11-05", timezone = "UTC")
  timezone = "UTC"
  provider = "USFS"
  unitID = "1051"
  baseUrl = "http://xxx.airsis.com/vision/common/CSVExport.aspx"

  # Read in AIRSIS .csv data
  fileString <- airsis_downloadData(
    startdate,
    enddate,
    timezone,
    provider,
    unitID,
    baseUrl
  )

}
