#' @export
#' @importFrom MazamaCoreUtils logger.trace logger.debug logger.warn logger.error
#'
#' @title Get WRCC unit IDs from the daily summary page
#'
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
#'   wrcc_getDailyUnitIDs()
#'
#' }, silent = FALSE)
#' }

wrcc_getDailyUnitIDs <- function(
  url = "https://wrcc.dri.edu/cgi-bin/smoke.pl"
) {

  logger.debug(" ----- wrcc_getDailyUnitIDs() ----- ")

  # ----- Validate parameters --------------------------------------------------

  MazamaCoreUtils::setIfNull(url, "https://wrcc.dri.edu/cgi-bin/smoke.pl")

  # ----- Get unit IDs ---------------------------------------------------------

  # NOTE:   Use chrome devtools to review the web page for link construction

  matchMatrix <-
    MazamaCoreUtils::html_getLinkUrls(url) %>%
    stringr::str_subset(pattern = "rawMAIN4\\.pl") %>%
    stringr::str_match(pattern = "^.*\\?id(....).*$")

  # ----- Return ---------------------------------------------------------------

  unitIDs <- matchMatrix[,2]

  return(unitIDs)

}
