#' @export
#' @importFrom MazamaCoreUtils logger.trace logger.debug logger.warn logger.error
#'
#' @title Download WRCC data
#'
#' @param startdate Desired start date (ISO 8601).
#' @param enddate Desired end date (ISO 8601).
#' @param timezone Olson timezone used to interpret dates (required).
#' @param unitID WRCC station identifier (will be upcased).
#' @param baseUrl Base URL for data queries.
#'
#' @description Download data for a particular station for the desired time period.
#' Data are returned as a single character string containing the WRCC output.
#'
#' Monitor unitIDs can be found at https://wrcc.dri.edu/cgi-bin/smoke.pl.
#'
#' @return String containing WRCC output.
#'
#' @references \href{https://wrcc.dri.edu/cgi-bin/smoke.pl}{Fire Cache Smoke Monitoring Archive}
#'
#' @examples
#' \dontrun{
#' # Fail gracefully if any resources are not available
#' try({
#'
#'   fileString <- wrcc_downloadData(20150701, 20150930, unitID = 'SM16')
#'
#'   fileString %>%
#'     readr::read_lines() %>%
#'     head(8)
#'
#' }, silent = FALSE)
#' }

wrcc_downloadData <- function(
  startdate = strftime(lubridate::now(tzone = "UTC"), "%Y010101", tz = "UTC"),
  enddate = strftime(lubridate::now(tzone = "UTC"), "%Y%m%d23", tz = "UTC"),
  timezone = "UTC",
  unitID = NULL,
  baseUrl = "https://wrcc.dri.edu/cgi-bin/wea_list2.pl"
) {

  logger.debug(" ----- wrcc_downloadData() ----- ")

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(startdate)
  MazamaCoreUtils::stopIfNull(enddate)
  MazamaCoreUtils::stopIfNull(timezone)
  MazamaCoreUtils::stopIfNull(unitID)

  # Get UTC times
  starttime <- MazamaCoreUtils::parseDatetime(startdate, timezone = "UTC")
  endtime <- MazamaCoreUtils::parseDatetime(enddate, timezone = "UTC")

  # ----- Download data --------------------------------------------------------

  logger.trace("Downloading WRCC data for unitID %s ...", unitID)

  # Create CGI parameters
  .params <- list(
    stn = toupper(unitID),
    smon = strftime(starttime,"%m",tz = "UTC"),
    sday = strftime(starttime,"%d",tz = "UTC"),
    syea = strftime(starttime,"%y",tz = "UTC"),
    emon = strftime(endtime,"%m",tz = "UTC"),
    eday = strftime(endtime,"%d",tz = "UTC"),
    eyea = strftime(endtime,"%y",tz = "UTC"),
    'Submit Info' = 'Submit Info',
    dfor = '04',
    src = 'W',
    miss = '08',
    flag = 'N',
    Dfmt = '01',
    Tfmt = '01',
    Head = '01',
    Deli = '01',
    unit = 'M',
    WsMon = '01',
    WsDay = '01',
    WeMon = '12',
    WeDay = '12',
    WsHou = '00',
    WeHou = '24',
    .cgifields = c('unit','flag','srce')
  )

  suppressWarnings({
    r <- httr::POST(baseUrl, body = .params)
  })

  if ( httr::http_error(r) ) {
    logger.error("WRCC data service failed for unitID: %s", unitID)
    logger.error("WRCC data service failed with: %s", httr::content(r))
    return("")
  }

  fileString <- httr::content(r, 'text', encoding = 'UTF-8')

  # ----- Return ---------------------------------------------------------------

  return(fileString)

}


# ===== DEBUGGING ==============================================================

if ( FALSE ) {


  library(MazamaCoreUtils)
  logger.setLevel(TRACE)

  library(AirMonitorIngest)

  startdate <- strftime(lubridate::now(tzone = "UTC"), "%Y010101", tz = "UTC")
  enddate <- strftime(lubridate::now(tzone = "UTC"), "%Y%m%d23", tz = "UTC")
  unitID <- "s328"
  baseUrl <- "https://wrcc.dri.edu/cgi-bin/wea_list2.pl"


}
