#' @export
#' @importFrom MazamaCoreUtils logger.trace logger.debug logger.warn logger.error
#'
#' @title Get WRCC unit IDs from the daily summary page
#'
#' @param enddate Desired end date (ISO 8601).
#' @param url URL with daily summary links.
#'
#' @description Parse the WRCC daily summary page to extract unit IDs for those
#' monitors that are currently reporting.
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
#'   wrcc_getCurrentUnitIDs()
#'
#' }, silent = FALSE)
#' }

wrcc_getCurrentUnitIDs <- function(
  enddate = lubridate::now(tzone = "UTC"),
  url = "https://wrcc.dri.edu/cgi-bin/smoke.pl"
) {

  logger.debug(" ----- wrcc_getCurrentUnitIDs() ----- ")

  # ----- Validate parameters --------------------------------------------------

  enddate <- MazamaCoreUtils::parseDatetime(enddate, timezone = "UTC")

  year <- strftime(enddate, "%y", tz = "UTC") # NOTE:  year without century
  month <- strftime(enddate, "%m", tz = "UTC")
  day <- strftime(enddate, "%d", tz = "UTC")

  url <- MazamaCoreUtils::setIfNull(url, "https://wrcc.dri.edu/cgi-bin/smoke.pl")

  # ----- Download current monitors page ---------------------------------------

  logger.trace("Downloading %s ...", url)

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
    return("")
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

  # ----- Return ---------------------------------------------------------------

  unitIDs <- matchMatrix[,2]

  return(unitIDs)

}
