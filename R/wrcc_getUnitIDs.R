#' @export
#' @importFrom MazamaCoreUtils logger.trace logger.debug logger.warn logger.error
#'
#' @title Get WRCC unit IDs
#'
#' @param startdate Desired start date (ISO 8601).
#' @param enddate Desired end date (ISO 8601).
#' @param by Increment by day, week or month.
#' @param url WRCC URL with daily summary links.
#'
#' @description Parse the WRCC daily web service response to extract unit IDs
#' for those monitors that were reporting on a particular date.
#'
#' With no arguments, this function will return the unitIDs of monitors that
#' are currently reporting.
#'
#' @return Vector of WRCC unit IDs.
#'
#' @references \href{https://wrcc.dri.edu/cgi-bin/smoke.pl}{Fire Cache Smoke Monitoring Archive}
#'
#' @examples
#' \dontrun{
#' # Fail gracefully if any resources are not available
#' try({
#'
#'   wrcc_getUnitIDs()
#'
#' }, silent = FALSE)
#' }

wrcc_getUnitIDs <- function(
  startdate = lubridate::now(tzone = "UTC"),
  enddate = NULL,
  by = c("day", "week", "month"),
  url = "https://wrcc.dri.edu/cgi-bin/smoke.pl"
) {

  logger.debug(" ----- wrcc_getUnitIDs() ----- ")

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::stopIfNull(startdate)
  by <- match.arg(by)

  url <- MazamaCoreUtils::setIfNull(url, "https://wrcc.dri.edu/cgi-bin/smoke.pl")

  startdate <- MazamaCoreUtils::parseDatetime(startdate, timezone = "UTC")

  if ( is.null(enddate) ) {
    enddate <- startdate
  } else {
    enddate <- MazamaCoreUtils::parseDatetime(enddate, timezone = "UTC")
  }

  datetimes <- seq(startdate, enddate, by = by)

  # ===== BEGIN LOOP ===========================================================

  idList <- list()

  # NOTE:  We can't just say "for (datetime in datetimes)" as this generates:
  # NOTE:    Error in as.POSIXlt.numeric(x, tz = tz) : 'origin' must be supplied

  for ( i in seq_along(datetimes) ) {

    datetime <- datetimes[i]

    year <- strftime(datetime, "%y", tz = "UTC") # NOTE:  year without century
    month <- strftime(datetime, "%m", tz = "UTC")
    day <- strftime(datetime, "%d", tz = "UTC")
    ymd <- paste0(year, month, day)

    # ----- Download current monitors page ---------------------------------------

    logger.trace("Downloading unitIDs for 20%s from %s ...", ymd, url)

    # Create CGI parameters
    .params <- list(
      mon = month,
      day = day,
      yea = year,
      Update = "Update"
    )

    suppressWarnings({
      r <- httr::POST(url, body = .params)
    })

    if ( httr::http_error(r) ) {
      logger.error("WRCC url failed with: %s", httr::content(r))
      next
    }

    # NOTE:  Non-standard return needs to be handled carefully
    # TODO:  Figure out which non-standard encoding is being used to encode "micro sign"
    fileString <-
      httr::content(r, as = "raw", type = "text/html", encoding = "UTF-8") %>%
      rawToChar()

    # ----- Get unit IDs ---------------------------------------------------------

    # NOTE:   Use chrome devtools to review the web page for link construction

    matchMatrix <-
      MazamaCoreUtils::html_getLinkUrls(fileString) %>%
      stringr::str_subset(pattern = "rawMAIN4\\.pl") %>%
      stringr::str_match(pattern = "^.*\\?id(....).*$")

    idList[[ymd]] <- matchMatrix[,2]

  }

  # ===== END LOOP =============================================================

  unitIDs <- unlist(idList) %>% unique() %>% sort()

  # ----- Return ---------------------------------------------------------------

  return(unitIDs)

}
